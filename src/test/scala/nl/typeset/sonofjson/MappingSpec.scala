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

  private def addIndex(x: JValue, index: Int) = x match {
    case JNumber(number) => JNumber(number + index)
    case other => other
  }

  "JValue" should {


    "should allow you to map using a function with two arguments on an Array" in {
      val xs = arr(3, 2, 1)
      val added = xs.map(addIndex)
      added(0) must be equalTo(3)
      added(1) must be equalTo(3)
      added(2) must be equalTo(3)
    }

    "should allow you to map using a function with only one argument, on an Array" in {
      val xs = arr(3, 2, 1)
      val doubled = xs.map(double)
      doubled(0) must be equalTo(6)
      doubled(1) must be equalTo(4)
      doubled(2) must be equalTo(2)
    }

    "should allow you map using a for comprehension, on an Array" in {
//      val xs = arr(3, 2, 1)
//      val doubled = for ((value: JValue, index: Int) <- xs) println(value)
//      doubled(0) must be equalTo(6)
//      doubled(1) must be equalTo(4)
//      doubled(2) must be equalTo(2)
      ko
    }.pendingUntilFixed("- which currently doesn't work")

  }

}
