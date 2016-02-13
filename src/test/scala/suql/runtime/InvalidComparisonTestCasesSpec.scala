package suql.runtime

import suql.ast._
import suql.codegen.Uncompiler
import suql.errors._
import suql.testing.UnitSpec

class InvalidComparisonTestCasesSpec extends UnitSpec {
  val uncompiler = new Uncompiler

  val stringOrderTestCases = List(
    GtExpr(BoolExpr(true), BoolExpr(false)),
    LtExpr(BoolExpr(true), BoolExpr(false)),

    GtExpr(StringExpr("hello"), StringExpr("world")),
    LtExpr(StringExpr("hello"), StringExpr("world")),

    NotExpr(StringExpr("hello")),
    NotExpr(IntExpr(123)),
    NotExpr(DecimalExpr(123.456))
  )

  val interpreter = new Interpreter

  implicit val runtimeContext = new RuntimeContext(new NullMemberResolver, new NullMemberResolver)

  for (testCase <- stringOrderTestCases) {
    it should s"throw a type exception when interpreting (${uncompiler.generate(testCase)})" in {
      a [SuqlRuntimeError] shouldBe thrownBy {
        interpreter.eval(testCase)
      }
    }
  }
}
