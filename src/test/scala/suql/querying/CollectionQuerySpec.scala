package suql.querying

import suql.testing.UnitSpec

class CollectionQuerySpec extends UnitSpec {

  case class Country(name: String, population: Int)

  val testCountries = List(
    Country("United Kingdom", 64100000),
    Country("France", 66030000),
    Country("United States", 316500000)
  )

  it should "filter countries with a population less than 300 million" in {
    val scalaListQuery: CollectionQuery = new CollectionQuery
    val filteredCountries: List[Country] = scalaListQuery.query("country.population > 300000000", testCountries)

    filteredCountries shouldBe testCountries.filter(country => country.population > 300000000)
  }

  it should "filter countries with a population greater than 300 million" in {
    val scalaListQuery: CollectionQuery = new CollectionQuery
    val filteredCountries: List[Country] = scalaListQuery.query("country.population < 300000001", testCountries)

    filteredCountries shouldBe testCountries.filter(country => country.population <= 300000000)
  }
}
