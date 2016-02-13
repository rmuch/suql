package suql.codegen

import suql.ast._
import suql.language.Symbols

class Uncompiler extends CodeGenerator[String] {
  override def generate(expr: Expr): String = expr match {
    case IdentifierExpr(identifier) => identifier
    case BoolExpr(boolValue) => boolValue.toString
    case IntExpr(value) => value.toString
    case DecimalExpr(value) => value.toString
    case StringExpr(value) => "\"" + s"$value" + "\"" // FIXME: Bug for a s"" format string containing escaped quotes?
    case ListExpr(List()) => "[]"
    case ListExpr(values) => "[ " + values.map(generate).mkString(", ") + " ]"
    case AndExpr(left, right) => s"${generate(left)} ${Symbols.AndOp} ${generate(right)}"
    case OrExpr(left, right) => s"${generate(left)} ${Symbols.OrOp} ${generate(right)}"
    case NotExpr(u) => s"!${generate(u)}"
    case EqExpr(left, right) => s"${generate(left)} ${Symbols.EqOp} ${generate(right)}"
    case NeqExpr(left, right) => s"${generate(left)} ${Symbols.NeqOp} ${generate(right)}"
    case GtExpr(left, right) => s"${generate(left)} ${Symbols.GtOp} ${generate(right)}"
    case LtExpr(left, right) => s"${generate(left)} ${Symbols.LtOp} ${generate(right)}"
    case CallExpr(name, args) => name + "(" + args.map(generate).mkString(", ") + ")"
    case _ => ???
  }
}
