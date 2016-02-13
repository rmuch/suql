package suql.runtime

import suql.ast.CallExpr
import suql.codegen.Uncompiler
import suql.testing.UnitSpec

import scala.collection.immutable.HashMap

class FunctionCallCasesSpec extends UnitSpec {
  val uncompiler = new Uncompiler

  val testCases = HashMap(
    "starts_with" -> List(
      (List(StringValue("hello world"), StringValue("hello")), BoolValue(true)),
      (List(StringValue("hello world"), StringValue("world")), BoolValue(false))
    ),
    "ends_with" -> List(
      (List(StringValue("hello world"), StringValue("hello")), BoolValue(false)),
      (List(StringValue("hello world"), StringValue("world")), BoolValue(true))
    ),
    "to_upper" -> List(
      (List(StringValue("Hello World")), StringValue("HELLO WORLD"))
    ),
    "to_lower" -> List(
      (List(StringValue("Hello World")), StringValue("hello world"))
    ),
    "int_to_string" -> List(
      (List(IntValue(495)), StringValue("495"))
    ),
    "string_to_int" -> List(
      (List(StringValue("495")), IntValue(495))
    ),
    "typeof" -> List(
      (List(StringValue("hello")), StringValue("string")),
      (List(IntValue(495)), StringValue("int"))
    )
  ).flatMap { case (functionName, itsTestCases) =>
    itsTestCases.map({ case (args, expectedReturn) =>
      (CallExpr(functionName, args map Value.valueToExpr), expectedReturn)
    })
  }

  for ((callExpr, expectedOutput) <- testCases) {
    it should s"return ($expectedOutput) for a call to (${uncompiler.generate(callExpr)})" in {
      val interpreter = new Interpreter
      implicit val runtimeContext = new RuntimeContext(new NullMemberResolver, new NullMemberResolver)

      val output = interpreter.eval(callExpr)

      output shouldBe expectedOutput
    }
  }

}
