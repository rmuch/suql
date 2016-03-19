package suql.runtime

import suql.ast._
import suql.errors.{SuqlRuntimeError, SuqlRuntimeException}

import scala.collection.immutable.HashMap

/** Abstract base class for the interpreter's internal representation of a value. */
abstract class Value

case class ListValue(list: List[Value]) extends Value
case class BoolValue(value: Boolean) extends Value
case class IntValue(value: Long) extends Value
case class DecimalValue(value: BigDecimal) extends Value
case class StringValue(value: String) extends Value
case class UnitValue() extends Value

object Value {
  /** Gets a friendly string representation of the type of a value.
    *
    * @param value The value to type check.
    * @return A friendly string representation of the type of the value.
    *
    * @throws SuqlRuntimeError Throws a SuqlRuntimeError if an unsupported type is provided as an argument.
    */
  def getTypeName(value: Value): String = value match {
    case ListValue(_) => "list"
    case BoolValue(_) => "bool"
    case IntValue(_) => "int"
    case DecimalValue(_) => "decimal"
    case StringValue(_) => "string"
    case UnitValue() => "unit"
    case _ => throw new SuqlRuntimeError(s"Unsupported type $value")
  }

  /** Converts a value to its string representation.
    *
    * @param value Value to convert.
    * @return String representation of the value.
    *
    * @throws SuqlRuntimeError Type error.
    */
  def valueToString(value: Value): String = value match {
    case ListValue(l) => "[ " + l.mkString(", ") + " ]"
    case BoolValue(b) => b.toString
    case IntValue(i) => i.toString
    case DecimalValue(d) => d.toString
    case StringValue(s) => s
    case _ => throw new SuqlRuntimeError(s"Unsupported type $value")
  }

  /** Converts a value to its BigDecimal representation.
    *
    * @param value Value to convert.
    * @return String representation of the value.
    *
    * @throws SuqlRuntimeError Type error.
    */
  def valueToBigDecimal(value: Value): BigDecimal = value match {
    // case BoolValue(b) => if (b) 1 else 0
    case IntValue(i) => i
    case DecimalValue(d) => d
    case _ => throw new SuqlRuntimeError(s"Unsupported type $value")
  }

  /** Tests the equivalence of two values. SUQL is statically typed and does not automatically coerce variables between
    * types. If the types do not match, an exception will be thrown.
    *
    * @param left Left side value.
    * @param right Right side value.
    * @return True if both sides are equal.
    *
    * @throws SuqlRuntimeException Throws a SuqlRuntimeException if a type error occurs.
    */
  def testEquivalence(left: Value, right: Value): Boolean = (left, right) match {
    case (BoolValue(a), BoolValue(b)) => a == b
    case (IntValue(a), IntValue(b)) => a == b
    case (DecimalValue(a), DecimalValue(b)) => a == b
    case (StringValue(a), StringValue(b)) => a == b
    case (ListValue(a), ListValue(b)) => if (a.length != b.length) false else a.zip(b).forall({case (x, y) => x == y})
    case _ => throw new SuqlRuntimeError(s"Unsupported equality comparison ($left, $right)")
  }

  /** Tests the equivalence of two values. If the type of the values don't match, they are converted to strings before
    * being compared.
    *
    * @param left Left side value.
    * @param right Right side value.
    * @return True if both sides are equal.
    *
    * @throws SuqlRuntimeException Throws a SuqlRuntimeException if a type error occurs.
    */
  def dynamicTestEquivalence(left: Value, right: Value): Boolean = valueToString(left) == valueToString(right)

  /** Tests the order of two values. If the right side is greater than the left side, this will return a positive
    * integer. If the left side is greater than the right side, this will return a negative integer. If the two values
    * are equal, this will return 0.
    *
    * @param left Left side value.
    * @param right Right side value.
    * @return 1 if left > right, 0 if left = right, -1 if left < right.
    *
    * @throws SuqlRuntimeException Throws a SuqlRuntimeException if a type error occurs.
    */
  def testOrder(left: Value, right: Value): Int = (left, right) match {
    case (IntValue(a), IntValue(b)) => a compareTo b
    case (DecimalValue(a), DecimalValue(b)) => a compare b
    case _ => throw new SuqlRuntimeError(s"Unsupported order comparison ($left, $right)")
  }

  /** Tests the order of two values. If the right side is greater than the left side, this will return a positive
    * integer. If the left side is greater than the right side, this will return a negative integer. If the two values
    * are equal, this will return 0.
    *
    * Both values are converted to [[BigDecimal]] before being compared.
    *
    * @param left Left side value.
    * @param right Right side value.
    * @return 1 if left > right, 0 if left = right, -1 if left < right.
    *
    * @throws SuqlRuntimeException Throws a SuqlRuntimeException if a type error occurs.
    */
  def dynamicTestOrder(left: Value, right: Value): Int = valueToBigDecimal(left) compare valueToBigDecimal(right)

  def valueToExpr(value: Value): Expr = value match {
    case ListValue(l) => ListExpr(l.map(valueToExpr))
    case BoolValue(a) => BoolExpr(a)
    case IntValue(a) => IntExpr(a)
    case DecimalValue(a) => DecimalExpr(a)
    case StringValue(a) => StringExpr(a)
    case UnitValue() => ???
    case _ => ???
  }

  def anyToValue(any: Any): Value = any match {
    case a if a == null => UnitValue()
    case l: List[Any] => ListValue(l.map(anyToValue))
    case b: Boolean => BoolValue(b)
    case i: Integer => IntValue(i.toLong)
    case i: Long => IntValue(i)
    case d: BigDecimal => DecimalValue(d)
    case d: Double => DecimalValue(d)
    case d: Float => DecimalValue(d.toDouble)
    case s: String => StringValue(s)
    case _ => ???
  }
}

trait MemberResolver {
  def exists(name: String): Boolean
  def get(name: String): Option[Value]
}

class NullMemberResolver extends MemberResolver {
  override def exists(name: String): Boolean = false
  override def get(name: String): Option[Value] = None
}

class MapMemberResolver(locals: HashMap[String, Value]) extends MemberResolver {
  override def exists(name: String): Boolean = locals.contains(name)
  override def get(name: String): Option[Value] = locals.get(name)
}

/** Class RuntimeContext contains runtime data. */
class RuntimeContext(val localMemberResolver: MemberResolver, val modelMemberResolver: MemberResolver) {
  /** This function initially attempts to check if a global variable has been defined in the current scope. If there is
    * no local variable defined with the given name, it then checks properties of the model object.
    *
    * @param identifier Identifier to look up.
    * @param runtimeContext Runtime context to resolve.
    * @return
    */
  def resolveIdentifier(identifier: String)(implicit runtimeContext: RuntimeContext): Option[Value] =
    localMemberResolver.get(identifier) orElse modelMemberResolver.get(identifier)

  /**
    * Applies a function by name, with the provided arguments.
    *
    * @param name The name of the function to call.
    * @param args An argument list of Value objects.
    * @return
    */
  def applyFunction(name: String, args: List[Value]): Value = Builtins.call(name, args)
}

/** Class Interpreter implements a simple AST walker to interpret a SUQL script. */
class Interpreter {
  /** Evaluates an expression tree.
    *
    * @param queryExpr Expression tree to evaluate.
    * @param runtimeContext Runtime context for resolving scope and entity variables and builtin functions.
    * @return The value of the evaluated expression tree.
    */
  def eval(queryExpr: Expr)(implicit runtimeContext: RuntimeContext): Value = {
    queryExpr match {
      case IdentifierExpr(identifier) => runtimeContext.resolveIdentifier(identifier) match {
        case Some(identifierValue) => identifierValue
        case None => throw new SuqlRuntimeError(s"${queryExpr.getLine}:${queryExpr.getCol}: variable '$identifier' " +
          s"is undefined")
      }
      case BoolExpr(boolValue) => BoolValue(boolValue)
      case IntExpr(value) => IntValue(value)
      case DecimalExpr(value) => DecimalValue(value)
      case StringExpr(value) => StringValue(value)
      case ListExpr(values) => ListValue(values.map(eval))
      case AndExpr(left, right) => (eval(left), eval(right)) match {
        case (BoolValue(a), BoolValue(b)) => BoolValue(a && b)
        case v => throw new SuqlRuntimeError(s"${queryExpr.getLine}:${queryExpr.getCol}: operator && (binary boolean and) " +
          s"requires two values of type BoolValue - was given ($v)")
      }
      case OrExpr(left, right) => (eval(left), eval(right)) match {
        case (BoolValue(a), BoolValue(b)) => BoolValue(a || b)
        case v => throw new SuqlRuntimeError(s"${queryExpr.getLine}:${queryExpr.getCol}: operator || (binary boolean or) " +
          s"requires two values of type BoolValue - was given ($v)")
      }
      case NotExpr(u) => eval(u) match {
        case BoolValue(b) => BoolValue(!b)
        case v => throw new SuqlRuntimeError(s"${queryExpr.getLine}:${queryExpr.getCol}: operator ! (unary boolean not) " +
          s"requires a value of type BoolValue - was given ($v)")
      }
      case EqExpr(left, right) => BoolValue(Value.testEquivalence(eval(left), eval(right)))
      case NeqExpr(left, right) => BoolValue(!Value.testEquivalence(eval(left), eval(right)))
      case GtExpr(left, right) => BoolValue(Value.testOrder(eval(left), eval(right)) > 0)
      case LtExpr(left, right) => BoolValue(Value.testOrder(eval(left), eval(right)) < 0)
      case CallExpr(name, args) => runtimeContext.applyFunction(name, args map eval)
      case _ => ???
    }
  }
}
