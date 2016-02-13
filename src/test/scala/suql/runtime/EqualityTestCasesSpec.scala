package suql.runtime

import suql.ast._
import suql.codegen.Uncompiler
import suql.testing.UnitSpec

import scala.collection.immutable.HashMap

class EqualityTestCasesSpec extends UnitSpec {
  val uncompiler = new Uncompiler

  val basicTestCases = HashMap(
    GtExpr(IntExpr(2), IntExpr(3)) -> BoolValue(false),
    GtExpr(IntExpr(3), IntExpr(2)) -> BoolValue(true),

    LtExpr(IntExpr(3), IntExpr(2)) -> BoolValue(false),
    LtExpr(IntExpr(2), IntExpr(3)) -> BoolValue(true)
  )

  // test cases exhibiting implicit conversion
  val dynamicCastTestCases = HashMap(
    EqExpr(StringExpr("3049"), IntExpr(3049)) -> BoolValue(true),
    EqExpr(IntExpr(5048), StringExpr("8405")) -> BoolValue(false)
  )

  val staticCastTestCases = HashMap(
    EqExpr(IntExpr(3045), IntExpr(3045)) -> BoolValue(true),
    EqExpr(IntExpr(3044), IntExpr(3045)) -> BoolValue(false)
  )

  val stringTestCases = HashMap(
    EqExpr(StringExpr("hello"), StringExpr("hello")) -> BoolValue(true),
    EqExpr(StringExpr("hello"), StringExpr("Hello")) -> BoolValue(false)
  )

  // test cases exhibiting implicit conversion
  val boolToIntImplicitTestCases = HashMap(
    EqExpr(BoolExpr(true), IntExpr(0)) -> BoolValue(false),
    EqExpr(BoolExpr(true), IntExpr(1)) -> BoolValue(true),
    EqExpr(BoolExpr(false), IntExpr(0)) -> BoolValue(true),
    EqExpr(BoolExpr(false), IntExpr(1)) -> BoolValue(false)
  )

  val listEqualityTestCases = HashMap(
    EqExpr(ListExpr(List(IntExpr(1), IntExpr(2), IntExpr(3))), ListExpr(List(IntExpr(1), IntExpr(2), IntExpr(3)))) -> BoolValue(true),
    EqExpr(ListExpr(List(IntExpr(1), IntExpr(2), IntExpr(3))), ListExpr(List(IntExpr(1), IntExpr(2), IntExpr(4)))) -> BoolValue(false)
  )

  val testCases = basicTestCases ++
    // dynamicCastTestCases ++
    staticCastTestCases ++
    stringTestCases ++
    // boolToIntImplicitTestCases ++
    listEqualityTestCases ++
    HashMap()

  val interpreter = new Interpreter

  implicit val runtimeContext = new RuntimeContext(new NullMemberResolver, new NullMemberResolver)

  for ((expressionTree, evaluatedValue) <- testCases) {
    it should s"evaluate (${uncompiler.generate(expressionTree)}) to (${Value.valueToString(evaluatedValue)})" in {
      interpreter.eval(expressionTree) shouldBe evaluatedValue
    }
  }
}
