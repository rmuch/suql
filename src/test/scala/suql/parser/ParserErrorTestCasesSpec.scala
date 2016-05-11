package suql.parser

import suql.testing.UnitSpec

import scala.collection.immutable.HashMap

class ParserErrorTestCasesSpec extends UnitSpec {
  val parser = new SuqlPackratParser

  val errorTestCases = HashMap(
    "\"hello world" -> "unterminated string literal"
  )

  for ((input, errorDescription) <- errorTestCases) {
    it should s"fail to parse $errorDescription" in {
      parser.parseStringE(input).isLeft shouldBe true
    }
  }
}
