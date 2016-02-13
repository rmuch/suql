package suql.parser

import org.scalatest.prop.TableDrivenPropertyChecks
import suql.ast._
import suql.testing.UnitSpec

import scala.collection.immutable.HashMap

class ParserTestCasesSpec extends UnitSpec with TableDrivenPropertyChecks {
  var parser = new SuqlPackratParser

  val simpleTestCases = HashMap(
    "hello(world)" -> CallExpr("hello", List(IdentifierExpr("world"))),
    "this(is, a, test)" -> CallExpr("this", List(IdentifierExpr("is"), IdentifierExpr("a"), IdentifierExpr("test"))),
    "call_with_numeric_args(3,2,1)" -> CallExpr("call_with_numeric_args", List(IntExpr(3), IntExpr(2), IntExpr(1))),
    "call_with_quoted_args(\"one\", \"two\")" ->
      CallExpr("call_with_quoted_args", List(StringExpr("one"), StringExpr("two"))),
    "call_with(under_scores(and_nested()))" ->
      CallExpr("call_with", List(CallExpr("under_scores", List(CallExpr("and_nested", List()))))),
    "1 > 2 > 3" -> GtExpr(IntExpr(1), GtExpr(IntExpr(2), IntExpr(3))),
    "identifier" -> IdentifierExpr("identifier"),
    "identifier_with_underscores" -> IdentifierExpr("identifier_with_underscores"),
    "!true" -> NotExpr(BoolExpr(true)),
    "true" -> BoolExpr(true),
    "false" -> BoolExpr(false),
    "123" -> IntExpr(123),
    "123.456" -> DecimalExpr(123.456),
    "\"hello world\"" -> StringExpr("hello world"),
    "[]" -> ListExpr(List()),
    "[1,2,3]" -> ListExpr(List(IntExpr(1), IntExpr(2), IntExpr(3))),
    "[1,\"b\",true]" -> ListExpr(List(IntExpr(1), StringExpr("b"), BoolExpr(true)))
  )

  val countryTestCases = HashMap(
    "country.population > 10000000" -> GtExpr(IdentifierExpr("country.population"), IntExpr(10000000)),
    """ends_with(country.name, "ia") && country.population > 7000000""" -> AndExpr(
      CallExpr("ends_with", List(IdentifierExpr("country.name"), StringExpr("ia"))),
      GtExpr(IdentifierExpr("country.population"), IntExpr(7000000)))
  )

  val validParserTestCases = simpleTestCases ++ countryTestCases

  for ((testCase, expressionTree) <- validParserTestCases) {
    it should s"parse ($testCase)" in {
      parser.parseString(testCase) shouldBe expressionTree
    }
  }
}
