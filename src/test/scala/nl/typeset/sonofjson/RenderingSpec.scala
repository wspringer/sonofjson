package nl.typeset.sonofjson.model

import org.specs2.mutable.Specification

class RenderingSpec extends Specification {

  "JValue objects" should {

    "be serializable to JSON" in {
      val json = obj(
        name = obj(
          first = "John",
          last = "Doe"
        ),
        numbers = arr(5, 4, 3, 2, 1)
      )
      println(render(json))
      val parsed = parse(render(json))
      parsed.name.first must be equalTo("John")
    }

    "preserve special characters" in {
      val s: JValue = "\tfoo"
      render(s) must be equalTo("\"\\tfoo\"")
    }

  }

}
