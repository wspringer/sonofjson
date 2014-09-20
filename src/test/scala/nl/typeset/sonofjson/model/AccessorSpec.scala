package nl.typeset.sonofjson.model

import org.specs2.mutable.Specification

class AccessorSpec extends Specification {

  val json = parse(
    """
      |{ "name" : { "first" : "John", "last" : "Doe" }, "age": 41, "married": true, "numbers": [5, 4, 3] }
    """.stripMargin.trim)

  "A JObject" should {

    "allow you to access properties by name" in {
      json.name.first.as[String] must be equalTo("John")
    }

    "consider undefined properties to be of type JUndefined" in {
      json.employee must be equalTo(JUndefined)
    }

    "throw an exception when you try to resolve an attribute on something that doesn't have any" in {
      json.employee.name must throwA[NotSupportedException]
    }

    "allow you to access numeric properties as Int" in {
      json.age.as[Int] must be equalTo(41)
    }

    "allow you to access boolean properties as Boolean" in {
      json.married.as[Boolean] must beTrue
    }

    "allow you to access BigDecimal properties as… BigDecimal" in {
      json.age.as[BigDecimal] must be equalTo(BigDecimal(41))
    }

    "leave type out in case there's no doubt" in {
      // IntelliJ is confused here, but Scala isn't
      val age: Int = json.age
      age must be equalTo(41)
    }

    "allow you to access array elements by position" in {
      json.numbers(0).as[Int] must be equalTo(5)
    }

    "allow you to set attributes" in {
      json.name.first = "Jack"
      json.name.first must be equalTo("Jack")
      json.age = 78
      json.age must be equalTo(78)
    }

  }

}
