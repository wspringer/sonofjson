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

class ParserSpec extends Specification {

  "The parser" should {

    "be able to work with String Interpolation  " in {
      json"""
        {
            "name" : "zhijia,.zhang",
            "age" : 25
        }
      """ must beLike {
        case JObject(obj) =>
          obj.get("name") must beSome
      }
    }

    "be able to parse an object" in {
      val json = """
        |{ "person" : { "first" : "John", "last": "Doe" } }
      """.stripMargin.trim
      parse(json) must beLike {
        case JObject(obj) =>
          obj.get("person") must beSome
      }
    }

    "do something sensible with parsing errors" in {
      parse("{ [ 0, 1, 2 ]}") must throwA[ParserException].like {
        case ParserException(msg, pos) =>
          pos.line must be equalTo(1)
          pos.column must be equalTo(3)
      }
    }

    "do something sensible when there's not enough input" in {
      parse("{ ") must throwA[ParserException].like {
        case ParserException(msg, pos) =>
          pos.line must be equalTo(1)
          pos.column must be equalTo(3)
      }
    }

  }

}
