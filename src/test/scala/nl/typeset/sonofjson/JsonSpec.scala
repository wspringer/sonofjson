package nl.typeset.sonofjson

import org.specs2.mutable.Specification

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
    }

  }

}
