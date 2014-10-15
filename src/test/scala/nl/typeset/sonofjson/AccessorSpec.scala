/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2014 Wilfred Springer
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package nl.typeset.sonofjson

import org.specs2.mutable.Specification

class AccessorSpec extends Specification {

  sequential

  val json = parse(
    """
      |{ "name" : { "first" : "John", "last" : "Doe" }, "age": 41, "married": true, "numbers": [5, 4, 3] }
    """.stripMargin.trim)

  "A JObject" should {

    "allow you to access properties by name" in {
      json.name.first.as[String] must be equalTo("John")
    }

    "allow you to access properties by name with array notation" in {
      json("name").first.as[String] must be equalTo("John")
    }

    "allow you to access nested properties by name with array notation" in {
      json.name("first").as[String] must be equalTo("John")
    }

    "allow you to modify nested properties by name with array notation" in {
      json.name("first") = "Charly"
      json.name.first.as[String] must be equalTo("Charly")
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

    "allow you to change array values" in {
      json.numbers(0) = 0
      json.numbers(0) must be equalTo(0)
    }

  }

}
