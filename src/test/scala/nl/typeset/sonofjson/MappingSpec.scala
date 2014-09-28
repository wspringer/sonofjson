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

/**
 * Not actually testing the map operation on JValue, since that has been removed.
 *
 * @see http://nxt.flotsam.nl/i-have-no-map
 */
class MappingSpec extends Specification {

  "JValue" should {

    "support as for mapping other things" in {
      val xs = arr(3, 2, 1)
      val doubled = xs.as[List[Int]].map(_ * 2)
      doubled.head must be equalTo(6)
    }

    "horribly fail in case you're trying something that won't work" in {
      val xs = arr(3, "foo", true)
      xs.as[List[Int]].map(_ * 2) must throwAn[Exception]
    }

    "support implicit creation of a JArray from a List[T <% JValue]" in {
      val value: JValue = List(1, 2, 4)
      value must beAnInstanceOf[JArray]
    }

    "support implicit creation of a JObject from a Map[String, T <% JValue]" in {
      val value: JValue = Map("first" -> 1, "second" -> true)
      value must beAnInstanceOf[JObject]
    }

  }

}
