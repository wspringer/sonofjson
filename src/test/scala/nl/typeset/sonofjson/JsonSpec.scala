package nl.typeset.sonofjson

import org.specs2.mutable.Specification

case class Name(first: String, last: String)

class JsonSpec extends Specification {

  val parsed = Json.parse(
    """
      |{ "name": { "first": "wilfred", "last": "springer" }, "numbers": [9, 8, 7, 6] }
    """.stripMargin.trim
  )

  "Json" should {

    "allow you to access data in a sensible way" in {
      parsed.name.first.as[String] must be equalTo("wilfred")
      parsed.numbers(0).as[Int] must be equalTo(9)
      parsed.name.as[Name] must be equalTo(Name("wilfred", "springer"))
    }

    "allow you to build JSON objects the easy way" in {
      import Json.json
      val obj = json.obj(
        first = "wilfred",
        last = "springer",
        age = 41,
        scala = true,
        address = json.obj(
          street = "Zuiderdiep 32",
          city = "Groningen"
        )
      )
      obj.first.as[String] must be equalTo("wilfred")
      obj.last.as[String] must be equalTo("springer")
      obj.age.as[Int] must be equalTo(41)
      obj.scala.as[Boolean] must be equalTo(true)
      obj.address.city.as[String] must be equalTo("Groningen")
      ok
    }

  }

}
