package suql.ast

case class TokenInformation(line: Int, col: Int)
// TODO: Use parser.Position or refactor it out into a common lib?
// TODO: Ensure combinator parser populates token information.

/** The base class that all AST nodes derive from. */
abstract class Expr {
  private[suql] var tokenInformation: Option[TokenInformation] = None

  private[suql] def setTokenInformation(implicit implicitTokenInformation: TokenInformation): Unit = {
    tokenInformation = Some(implicitTokenInformation)
  }

  /** Gets token information for this AST node.
    *s
    * @return Token information for this AST node.
    */
  def getTokenInformation: Option[TokenInformation] = tokenInformation

  /** Gets the line number where this AST node was parsed.
    *
    * @return The line number where this AST node was parsed.
    */
  def getLine = getTokenInformation match {
    case Some(TokenInformation(line, col)) => line
    case None | _ => -1
  }

  /** Gets the column number where this AST node was parsed.
    *
    * @return The column number where this AST node was parsed.
    */
  def getCol = getTokenInformation match {
    case Some(TokenInformation(line, col)) => col
    case None | _ => -1
  }
}

case class IdentifierExpr(identifier: String) extends Expr
case class BoolExpr(boolValue: Boolean) extends Expr
case class IntExpr(value: Long) extends Expr
case class DecimalExpr(value: BigDecimal) extends Expr
case class StringExpr(value: String) extends Expr
case class ListExpr(exprs: List[Expr]) extends Expr
case class AndExpr(left: Expr, right: Expr) extends Expr
case class OrExpr(left: Expr, right: Expr) extends Expr
case class NotExpr(expr: Expr) extends Expr
case class EqExpr(left: Expr, right: Expr) extends Expr
case class NeqExpr(left: Expr, right: Expr) extends Expr
case class GtExpr(left: Expr, right: Expr) extends Expr
case class LtExpr(left: Expr, right: Expr) extends Expr
case class CallExpr(name: String, args: List[Expr]) extends Expr
