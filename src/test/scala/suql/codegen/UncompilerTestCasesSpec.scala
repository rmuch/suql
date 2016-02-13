package suql.codegen

import suql.ast._
import suql.testing.UnitSpec

import scala.collection.immutable.HashMap

class UncompilerTestCasesSpec extends UnitSpec {
  val uncompiler = new Uncompiler

  val primitiveTestCases = HashMap(
    IntExpr(104) -> "104",
    DecimalExpr(123.45) -> "123.45",
    BoolExpr(true) -> "true",
    BoolExpr(false) -> "false",
    StringExpr("hello world") -> "\"hello world\""
  )

  for ((testCase, expectedOutput) <- primitiveTestCases) {
    it should s"compile primitive expression ($testCase) as ($expectedOutput)" in {
      uncompiler.generate(testCase) shouldBe expectedOutput
    }
  }

  val complexTestCases = HashMap( // TODO: Share with other tests
    "hello(world)" -> CallExpr("hello", List(IdentifierExpr("world"))),
    "this(is, a, test)" -> CallExpr("this", List(IdentifierExpr("is"), IdentifierExpr("a"), IdentifierExpr("test"))),
    "call_with_numeric_args(3, 2, 1)" -> CallExpr("call_with_numeric_args", List(IntExpr(3), IntExpr(2), IntExpr(1))),
    "call_with_quoted_args(\"one\", \"two\")" ->
      CallExpr("call_with_quoted_args",List(StringExpr("one"), StringExpr("two"))),
    "call_with(under_scores(and_nested()))" ->
      CallExpr("call_with", List(CallExpr("under_scores", List(CallExpr("and_nested", List()))))),
    "1 > 2 > 3" -> GtExpr(IntExpr(1), GtExpr(IntExpr(2), IntExpr(3))),
    "identifier" -> IdentifierExpr("identifier"),
    "identifier_with_underscores" -> IdentifierExpr("identifier_with_underscores")
  )

  for ((expectedOutput, testCase) <- complexTestCases) {
    it should s"compile expression ($testCase) as ($expectedOutput)" in {
      uncompiler.generate(testCase) shouldBe expectedOutput
    }
  }
}
