package suql.runtime

import suql.testing.UnitSpec

import scala.collection.immutable.HashMap

class BuiltinsSpec extends UnitSpec {

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
  )

  for ((func, vectors) <- testCases) {
    for ((arguments, expectedOutput) <- vectors) {
      it should s"return $expectedOutput for a call to $func with args $arguments" in {
        val builtins = new BuiltinsImpl {}

        builtins.call(func, arguments) shouldBe expectedOutput
      }
    }
  }
}
