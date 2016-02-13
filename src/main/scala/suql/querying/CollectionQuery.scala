package suql.querying

import suql.parser.SuqlPackratParser
import suql.runtime._

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}

/** Resolves model members based on getter and setter methods on a JVM object. */
class ObjectMemberResolver[A: ru.TypeTag: ClassTag] extends MemberResolver {
  lazy val properties = ru.typeOf[A].members.view.filter{!_.isMethod}.toList

  private var instance: Option[A] = None
  def setInstance(a: A) = instance = Some(a)

  override def exists(name: String): Boolean = getCaseAccessors[A].count{_.fullName == name} > 0

  override def get(fullName: String): Option[Value] = {
    // TODO: nested members
    val memberName = fullName.split("\\.").reverse.head

    val caseAccessors: List[ru.MethodSymbol] = getCaseAccessors[A]
    val caseAccessor: ru.MethodSymbol = caseAccessors.filter{ _.name.toString == memberName }.head

    val runtimeMirror: ru.Mirror = ru.runtimeMirror(instance.get.getClass.getClassLoader)

    // TODO: could be reflectMethod.apply too
    val caseAccessorValue: Any = runtimeMirror.reflect[A](instance.get).reflectField(caseAccessor).get

    Option(Value.anyToValue(caseAccessorValue))
  }

  def getCaseAccessors[A: ru.TypeTag : ClassTag] = {
    ru.typeOf[A].members.collect{ case m: ru.MethodSymbol if m.isCaseAccessor => m }.toList
  }
}

class CollectionQuery {
  private val parser = new SuqlPackratParser()
  private val runtime = new Interpreter()

  def query[A: ru.TypeTag : ClassTag](query: String, dataSet: List[A]): List[A] = {
    val nullMemberResolver: NullMemberResolver = new NullMemberResolver
    val objectMemberResolver: ObjectMemberResolver[A] = new ObjectMemberResolver[A]

    implicit val runtimeContext = new RuntimeContext(nullMemberResolver, objectMemberResolver)

    val parsedQuery = parser.parseString(query)

    dataSet filter { q =>
      objectMemberResolver.setInstance(q)

      runtime.eval(parsedQuery) match {
        case BoolValue(value) => value
        case v => throw new Exception("A query expression should return a boolean expression, but in this case, it " +
          s"returned a $v")
      }
    }
  }

  def queryAsync[A: ru.TypeTag : ClassTag](query: String, dataSet: List[A])
                                          (implicit executionContext: ExecutionContext): Future[List[A]] = {
    Future {
      this.query(query, dataSet)
    }
  }
}

// TODO: provide query method implicitly