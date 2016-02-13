package suql.runtime

import suql.errors.{SuqlRuntimeException, SuqlRuntimeError}

import scala.collection.immutable.HashMap
import scala.util.parsing.combinator.RegexParsers

private class FunctionSignature(val name: String, val args: Map[String, String], val returns: String) {
  override def toString: String = {
    val argList = args.map({ case (a, b) => s"$a $b" }).mkString(", ")
    s"$returns $name($argList)"
  }
}

private object FunctionSignature {
  def apply(signatureString: String): FunctionSignature = {
    val parser = new FunctionSignatureParser
    parser.parse(signatureString) match {
      case Some(signatureObject) => signatureObject
      case None => ???
    }
  }
}

private class FunctionSignatureParser {
  private class SignatureParserInternal extends RegexParsers {
    private def identifier: Parser[String] = """[A-Za-z0-9_]+""".r

    private def argumentPair: Parser[(String, String)] =
      (identifier ~ identifier) ^^ { case typeName ~ name => (typeName, name) } |
        identifier ^^ { case typeName => (typeName, "") }

    private def argumentList: Parser[Map[String, String]] =
      ("(" ~> repsep(argumentPair, ",") <~ ")") ^^ { args => args.toMap }

    def functionSignature: Parser[FunctionSignature] =
      (identifier ~ identifier ~ argumentList) ^^ { case returnType ~ name ~ args => new FunctionSignature(name, args, returnType) }
  }

  private val signatureParser = new SignatureParserInternal

  def parse(signature: String): Option[FunctionSignature] = {
    val parseResult = signatureParser.parse(signatureParser.functionSignature, signature)

    Option(parseResult.getOrElse(null))
  }
}

trait BuiltinProviderBase {
  private[runtime] type Builtin = List[Value] => Value

  private[runtime] var builtinMap: scala.collection.mutable.HashMap[FunctionSignature, Builtin]

  private def builtinNameMap = builtinMap.map({ case (signature, _) => (signature.name, signature) }) // TODO: Memoize

  def exists(name: String): Boolean = builtinNameMap.get(name).isDefined

  def call(name: String, args: List[Value]): Value = {
    builtinNameMap.get(name) match {
      case Some(signature) => builtinMap.get(signature) match {
        case Some(builtin) => builtin(args)
        case None => throw new SuqlRuntimeError(s"Builtin function with name $name exists in the name map but not " +
          s"the function map.")
      }
      case None => throw new SuqlRuntimeException(s"Builtin function with name $name does not exist.")
    }
  }

  builtinMap += (
    FunctionSignature("bool test1(string, string)") -> null,
    FunctionSignature("bool test2(string, string)") -> null
  )
}

trait BuiltinErrorHandling {
  def !!! : Nothing = throw new SuqlRuntimeError("invalid argument")
}

trait StringBuiltins extends BuiltinErrorHandling {
  private[suql] def startsWith(args: List[Value]): Value = args match {
    case List(StringValue(a), StringValue(b)) => BoolValue(a startsWith b)
    case _ => !!!
  }

  private[suql] def endsWith(args: List[Value]): Value = args match {
    case List(StringValue(a), StringValue(b)) => BoolValue(a endsWith b)
    case _ => !!!
  }

  private[suql] def toUpper(args: List[Value]): Value = args match {
    case List(StringValue(a)) => StringValue(a.toUpperCase)
    case _ => !!!
  }

  private[suql] def toLower(args: List[Value]): Value = args match {
    case List(StringValue(a)) => StringValue(a.toLowerCase)
    case _ => !!!
  }
}

trait ConversionBuiltins extends BuiltinErrorHandling {
  private[suql] def intToString(args: List[Value]): Value = args match {
    case List(IntValue(a)) => StringValue(a.toString)
    case _ => !!!
  }

  private[suql] def stringToInt(args: List[Value]): Value = args match {
    case List(StringValue(a)) => IntValue(a.toLong)
    case _ => !!!
  }

  private[suql] def typeOf(args: List[Value]): Value = args match {
    case List(v: Value) => StringValue(Value.getTypeName(v))
    case _ => !!!
  }
}

trait DebugBuiltins extends BuiltinProviderBase with BuiltinErrorHandling {
  private[suql] def break(args: List[Value]): Value = args match {
    case List() => !!!
    case _ => !!!
  }

  builtinMap += (
    FunctionSignature("unit break()") -> break
  )
}

trait BuiltinsImpl extends StringBuiltins with ConversionBuiltins {
  private type Builtin = List[Value] => Value

  private val builtinMap: HashMap[FunctionSignature, Builtin] = HashMap(
    FunctionSignature("bool starts_with(string, string)") -> startsWith,
    FunctionSignature("bool ends_with(string, string)") -> endsWith,
    FunctionSignature("string to_upper(string)") -> toUpper,
    FunctionSignature("string to_lower(string)") -> toLower,
    FunctionSignature("string int_to_string(int)") -> intToString,
    FunctionSignature("int string_to_int(string)") -> stringToInt,
    FunctionSignature("string typeof(any)") -> typeOf
  )

  private val builtinNameMap = builtinMap.map({ case (signature, _) => (signature.name, signature) })

  def exists(name: String): Boolean = builtinNameMap.get(name).isDefined

  def call(name: String, args: List[Value]): Value = {
    builtinNameMap.get(name) match {
      case Some(signature) => builtinMap.get(signature) match {
        case Some(builtin) => builtin(args)
        case None => throw new SuqlRuntimeError(s"Builtin function with name $name exists in the name map but not " +
          s"the function map.")
      }
      case None => throw new SuqlRuntimeException(s"Builtin function with name $name does not exist.")
    }
  }
}

object Builtins extends BuiltinsImpl
