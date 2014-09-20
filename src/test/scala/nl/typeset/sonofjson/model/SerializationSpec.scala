package nl.typeset.sonofjson.model

import org.specs2.mutable.Specification

class SerializationSpec extends Specification {

  "JValue objects" should {

    "be serializable to JSON" in {
      val json = obj(
        name = obj(
          first = "John",
          last = "Doe"
        ),
        numbers = arr(5, 4, 3, 2, 1)
      )
      println(json.asJson)
      val parsed = parse(json.asJson)
      parsed.name.first must be equalTo("John")
    }

    "preserve special characters" in {
      val s: JValue = "\tfoo"
      s.asJson must be equalTo("\"\\tfoo\"")
    }

  }

}
