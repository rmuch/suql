package suql.parser

import suql.ast.Expr

trait Parser {
  def ___parse(input: String): Either[ParseError, Expr]

  def ___parseOrThrow(input: String): Expr = {
    ___parse(input) match {
      case Left(error) => throw new ParseException(error)
      case Right(expr) => expr
    }
  }
}
