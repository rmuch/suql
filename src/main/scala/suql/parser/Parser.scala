package suql.parser

import suql.ast.Expr

trait Parser {
  def ___parse(input: String): Expr
}
