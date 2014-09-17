package nl.typeset.sonofjson

import org.specs2.mutable.Specification

case class Name(first: String, last: String)

class JsonSpec extends Specification {

  val json = Json.parse(
    """
      |{ "name": { "first": "wilfred", "last": "springer" }, "numbers": [9, 8, 7, 6] }
    """.stripMargin.trim
  )

  "Json" should {

    "allow you to access data in a sensible way" in {
      json.name.first.as[String] must be equalTo("wilfred")
      json.numbers(0).as[Int] must be equalTo(9)
      json.name.as[Name] must be equalTo(Name("wilfred", "springer"))
    }

  }

}
