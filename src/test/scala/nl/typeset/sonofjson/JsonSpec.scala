package nl.typeset.sonofjson

import org.specs2.mutable.Specification

case class Name(first: String, last: String)

class JsonSpec extends Specification {

  val parsed = Json.parse(
    """
      |{ "name": { "first": "John", "last": "Doe" }, "numbers": [9, 8, 7, 6] }
    """.stripMargin.trim
  )

  "Json" should {

    "allow you to access data in a sensible way" in {
      parsed.name.first.as[String] must be equalTo("John")
      parsed.numbers(0).as[Int] must be equalTo(9)
      parsed.name.as[Name] must be equalTo(Name("John", "Doe"))
    }

    "allow you to build JSON objects the easy way" in {
      import Json.json
      val obj = json.obj(
        first = "John",
        last = "Doe",
        age = 41,
        scala = true,
        address = json.obj(
          street = "Columbus",
          city = "San Francisco"
        )
      )
      obj.first.as[String] must be equalTo("John")
      obj.last.as[String] must be equalTo("Doe")
      obj.age.as[Int] must be equalTo(41)
      obj.scala.as[Boolean] must be equalTo(true)
      obj.address.city.as[String] must be equalTo("San Francisco")
      ok
    }

  }

}
