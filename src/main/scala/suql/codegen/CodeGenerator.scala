package suql.codegen

import suql.ast._

/** Takes a single expression or an expression tree as input, returning generated code represented as type [[A]].
  *
  * @tparam A Output type.
  */
trait CodeGenerator[A] {
  def generate(expr: Expr): A
}