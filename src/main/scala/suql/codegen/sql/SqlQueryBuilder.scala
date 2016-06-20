package suql.codegen.sql

import suql.ast._

trait AnsiSqlDialectTokens {
  val varLeftQuoteChar = "'"
  val varRightQuoteChar = "'"
  val stringQuoteChar = "\""
}

trait SqlServerDialectTokens extends AnsiSqlDialectTokens {
  override val varLeftQuoteChar: String = "["
  override val varRightQuoteChar: String = "]"
  override val stringQuoteChar: String = "\'" // TODO
}

trait MySqlDialectTokens extends AnsiSqlDialectTokens {
  override val varRightQuoteChar: String = "`"
  override val varLeftQuoteChar: String = "`"
  override val stringQuoteChar: String = "\""
}

trait PostgresDialectTokens extends AnsiSqlDialectTokens {
  override val varLeftQuoteChar: String = "'"
  override val varRightQuoteChar: String = "'"
  override val stringQuoteChar: String = "\""
}

class SqlQueryBuilder extends AnsiSqlDialectTokens {
  private def translateCall(callExpr: CallExpr): String = ??? // "TODO_CALL()" // todo

  private def escapeIdentifier(id: String): String = {
    id.split("\\.").tail.head.filter { char => char.isLetter || char.isDigit }
  }

  private def escapeString(s: String): String = {
    val escapables = List("\'") // TODO

    val escaped = escapables.foldRight(s)({case (e: String, b: String) => b.replace(e, "\\" + e)})

    escaped
  }

  private def generateWhere(expr: Expr): String = expr match {
    case IdentifierExpr(identifier) => s"$varLeftQuoteChar${escapeIdentifier(identifier)}$varRightQuoteChar"
    case BoolExpr(boolValue) => boolValue.toString
    case IntExpr(value) => value.toString
    case DecimalExpr(value) => value.toString
    case StringExpr(value) => s"$stringQuoteChar${escapeString(value)}$stringQuoteChar"
    case ListExpr(List()) => ???
    case ListExpr(values) => ???
    case AndExpr(left, right) => s"${generateWhere(left)} AND ${generateWhere(right)}"
    case OrExpr(left, right) => s"${generateWhere(left)} OR ${generateWhere(right)}"
    case NotExpr(u) => s"!${generateWhere(u)}"
    case EqExpr(left, right) => s"${generateWhere(left)} = ${generateWhere(right)}"
    case NeqExpr(left, right) => s"${generateWhere(left)} != ${generateWhere(right)}"
    case GtExpr(left, right) => s"${generateWhere(left)} > ${generateWhere(right)}"
    case LtExpr(left, right) => s"${generateWhere(left)} < ${generateWhere(right)}"
    case callExpr@CallExpr(name, args) => translateCall(callExpr)
    case _ => ???
  }

  def generateSqlQuery(table: String, selectColumns: Option[List[String]], queryExpression: Expr): String = {
    val columns = selectColumns match {
      case Some(columnNames) => columnNames.mkString(", ")
      case None => "*"
    }
    val query = generateWhere(queryExpression)

    s"SELECT ($columns) FROM $varLeftQuoteChar$table$varRightQuoteChar WHERE $query"
  }
}
