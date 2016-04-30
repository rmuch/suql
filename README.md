# simple user query language

suql is a small language for writing simple data queries. The intended use case
is to give users of a web application the ability to perform predicate-based
filtering on a table, similar to JQL for JIRA. suql has an interpreter that lets
it query collections of Scala objects, and also a compiler that converts the
suql code to a SQL or MongoDB query.

## Language features

 * Everything is an expression, and a query is a single expression
 * Equality and order comparisons (`=`, `!=`, `>`, `<`)
 * Logical negation (`!`) and boolean operators (`&&`, `||`)

The language deliberately omits a few features to maximise security, simplicity
and portability.

 * No variables
 * No user-defined functions
 * No ability to assign, write or update data - purely read-only

## An example: querying countries

Let's say we have a table of countries, like so:

country.name   | country.continent | country.population
---------------|-------------------|-------------------
United Kingdom | Europe            | 64100000
France         | Europe            | 66030000
United States  | North America     | 316500000
...            | ...               | ...

If we want to query all of the countries in Europe - the query would look like
this:

```
country.continent = "Europe"
```

Boolean logic allows us to write more complicated queries, like this:

```
country.continent = "North America" && country.population > 100000000
```

Parentheses can be used to disambiguate expressions:

```
(country.continent = "North America") && (country.population > 100000000)
```

This example shows how to perform a query from Scala code:

```scala
case class Country(name: String, continent: String, population: Int)

val countries = List(
  Country("United Kingdom", "Europe", 64100000),
  Country("France", "Europe", 66030000),
  Country("United States", "North America", 316500000)
)

val collectionQuery: CollectionQuery = new CollectionQuery
val bigCountriesInNorthAmerica = collectionQuery.query("country.continent = \"North America\" && country.population > 100000000", testCountries)
```

## Adding targets

To write a new code generation target, implement the `CodeGenerator`
trait. An example, which turns a suql syntax tree back into suql code, is
implemented in the `Uncompiler` class.
