package suql.codegen.sql

import suql.parser.SuqlPackratParser
import suql.testing.UnitSpec

import scala.collection.immutable.HashMap

class SqlQueryBuilderSpec extends UnitSpec {

  /**
    * CREATE TABLE country (
    *   country_id SERIAL PRIMARY KEY NOT NULL,
    *   name VARCHAR(255) NOT NULL,
    *   population INT NOT NULL
    * );
    *
    */

  val countryQueries = HashMap(
    "country.name = \"Cyprus\"" -> "SELECT (*) FROM [country] WHERE [name] = 'Cyprus'"
  )

  val queryBuilder = new SqlQueryBuilder with SqlServerDialectTokens
  val parser = new SuqlPackratParser

  for ((suqlQuery, sqlQuery) <- countryQueries) {
    it should s"generate expected SQL for query $suqlQuery" in {
      val expressionTree = parser.___parse(suqlQuery).right.get
      val generatedQuery = queryBuilder.generateSqlQuery("country", None, expressionTree)

      generatedQuery shouldBe sqlQuery
    }
  }
}
