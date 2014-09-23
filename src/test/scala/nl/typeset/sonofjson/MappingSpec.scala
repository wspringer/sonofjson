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

class MappingSpec extends Specification {

  private def double(x: JValue): JValue = x match {
    case JNumber(number) => JNumber(number * 2)
    case other => other
  }


  "JValue" should {

    "allow you to map using a function with only one argument, on an JArray" in {
      val xs = arr(3, 2, 1)
      val doubled = xs.map(double)
      doubled must beAnInstanceOf[JArray]
      doubled(0) must be equalTo(6)
      doubled(1) must be equalTo(4)
      doubled(2) must be equalTo(2)
    }

    "allow you to map using a for comprehension, on an JArray" in {
      val xs = arr(3, 2, 1)
      val doubled = for (x <- xs) yield JNumber(x.as[Int] * 2)
      doubled must beAnInstanceOf[JArray]
      doubled(0) must be equalTo(6)
    }

    "allow you to map to something else than a JArray" in {
      val xs = arr(3, 2, 1)
      val ys = for (x <- xs) yield List.fill(x.as[Int])("foo") // Not something we can convert into JArray
      ys must beAnInstanceOf[List[List[String]]]
    }

    "act upon JObject values as well" in {
      val xs = obj(foo = 2, bar = 3)
      val doubled = for (x <- xs) yield JNumber(x.as[Int] * 2)
      doubled must beAnInstanceOf[JArray]
      doubled(0) must be equalTo(4)
      doubled(1) must be equalTo(6)
    }

    "not map to a JArray if the elements getting generated are not JValues" in {
      val xs = arr(3, 2, 1)
      val ys: Seq[Int] = for (x <- xs) yield x.as[Int] * 2
      ys must beAnInstanceOf[Seq[Int]]
    }

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
