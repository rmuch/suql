package suql.parser

import suql.ast._

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

/** SUQL expression parser implemented using Scala's parsing combinator library. */
class SuqlPackratParser extends Parser with RegexParsers with PackratParsers {
  private val identifierRegex = """[A-Za-z\-_]+""".r
  private val identifierRegex2 = """[A-Za-z\-_\.]+""".r

  private lazy val id: PackratParser[IdentifierExpr] = identifierRegex2 ^^ { s => IdentifierExpr(s) }
  private lazy val bool: PackratParser[BoolExpr] = """(true|false)""".r ^^ { s => BoolExpr(s.toBoolean) }
  private lazy val int: PackratParser[IntExpr] = """[0-9]+""".r ^^ { s => IntExpr(s.toLong) }
  private lazy val dec: PackratParser[DecimalExpr] = """[0-9]+\.[0-9]+""".r ^^ { s => DecimalExpr(BigDecimal(s)) }
  private lazy val str: PackratParser[StringExpr] = "\"" ~> """[A-Za-z0-9 ]+""".r <~ "\"" ^^ { s => StringExpr(s) }

  private lazy val not: PackratParser[NotExpr] = "!" ~> expr ^^ { expr => NotExpr(expr) }

  private lazy val eq: PackratParser[EqExpr] = (expr ~ "=" ~ expr) ^^ { case left ~ op ~ right => EqExpr(left, right) }
  private lazy val neq: PackratParser[NeqExpr] = (expr ~ "!=" ~ expr) ^^ { case left ~ op ~ right => NeqExpr(left, right) }
  private lazy val gt: PackratParser[GtExpr] = (expr ~ ">" ~ expr) ^^ { case left ~ op ~ right => GtExpr(left, right) }
  private lazy val lt: PackratParser[LtExpr] = (expr ~ "<" ~ expr) ^^ { case left ~ op ~ right => LtExpr(left, right) }

  private lazy val and: PackratParser[AndExpr] = (expr ~ "&&" ~ expr) ^^ { case left ~ op ~ right => AndExpr(left, right) }
  private lazy val or: PackratParser[OrExpr] = (expr ~ "||" ~ expr) ^^ { case left ~ op ~ right => OrExpr(left, right) }

  private lazy val list: PackratParser[ListExpr] = ("[" ~> repsep(expr, ",") <~ "]") ^^ { case exprs => ListExpr(exprs) }

  private lazy val call: PackratParser[CallExpr] =
    (identifierRegex ~ "(" ~ repsep(expr, ",") ~ ")") ^^ { case name ~ lbr ~ csl ~ rbr => CallExpr(name, csl) }

  private lazy val uexpr: PackratParser[Expr] = pexpr1 | npexpr | call | list | not | bool | dec | int | str | id

  private lazy val npexpr: PackratParser[Expr] = eq | neq

  private lazy val pexpr1: PackratParser[Expr] = and | pexpr2
  private lazy val pexpr2: PackratParser[Expr] = or | pexpr3
  private lazy val pexpr3: PackratParser[Expr] = gt | pexpr4
  private lazy val pexpr4: PackratParser[Expr] = lt

  private lazy val expr: PackratParser[Expr] = uexpr | "(" ~> uexpr <~ ")"

  /** Attempts to parse a SUQL query expression represented as a string.
    *
    * @param string SUQL query expression.
    * @return Parsed SUQL program.
    */
  def parseString(string: String): Expr = {
    val packratReader = new PackratReader[Char](new CharSequenceReader(string))
    val parseResult = parse(expr, packratReader)

    println(parseResult)

    parseResult.get
  }

  def parseStringE(string: String): Either[String, Expr] = {
    val packratReader = new PackratReader[Char](new CharSequenceReader(string))
    val parseResult: ParseResult[Expr] = parse(expr, packratReader)

    println(parseResult)

    parseResult match {
      case Success(result, next) => Right(result)
      case NoSuccess(failureMessage, next) => Left(failureMessage)
    }
  }

  override def ___parse(input: String): Expr = parseString(input)
}
