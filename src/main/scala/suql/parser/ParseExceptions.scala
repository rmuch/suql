package suql.parser

import suql.errors.SuqlThrowable

// ParseError below doesn't currently derive from the Exception hierarchy, and Position is common that it can be moved
// out to somewhere else and used between the AST and parse error modules.
case class Position(line: Int, column: Int)
case class ParseError(position: Position, message: String)

case class ParseException(error: ParseError) extends Exception with SuqlThrowable