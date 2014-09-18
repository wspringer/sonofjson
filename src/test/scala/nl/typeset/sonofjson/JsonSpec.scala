package nl.typeset.sonofjson

import org.specs2.mutable.Specification

case class Name(first: String, last: String)

class JsonSpec extends Specification {

  val person = Json.parse(
    """
      |{ "name": { "first": "John", "last": "Doe" }, "numbers": [9, 8, 7, 6] }
    """.stripMargin.trim
  )

  "Json" should {

    "allow you to access data in a sensible way" in {
      person.name.first.as[String] must be equalTo("John")
      person.numbers(0).as[Int] must be equalTo(9)
      person.name.as[Name] must be equalTo(Name("John", "Doe"))
    }

    "allow you to build JSON objects the easy way" in {
      val person = obj(
        first = "John",
        last = "Doe",
        age = 41,
        scala = true,
        address = obj(
          street = "Columbus",
          city = "San Francisco"
        ),
        numbers = arr(1, 4, 1, 111, 21)
      )
      person.first.as[String] must be equalTo("John")
      person.last.as[String] must be equalTo("Doe")
      person.age.as[Int] must be equalTo(41)
      person.scala.as[Boolean] must be equalTo(true)
      person.address.city.as[String] must be equalTo("San Francisco")
      person.numbers(0).as[Int] must be equalTo(1)
      ok
    }

  }

}
