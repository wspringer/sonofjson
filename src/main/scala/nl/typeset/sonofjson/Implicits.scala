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

package nl.typeset.sonofjson.model

import scala.reflect.runtime.universe._

trait Implicits {

  implicit val IntDecoder: Decoder[Int] = {
    case JNumber(number) => number.toInt
  }

  implicit val DoubleDecoder: Decoder[Double] = {
    case JNumber(number) => number.toDouble
  }

  implicit val FloatDecoder: Decoder[Float] = {
    case JNumber(number) => number.toFloat
  }

  implicit val LongDecoder: Decoder[Long] = {
    case JNumber(number) => number.toLong
  }

  implicit val StringDecoder: Decoder[String] = {
    case JString(str) => str
    case JNumber(number) => number.toString()
    case JNull => "null"
    case JBool(bool) => bool.toString
  }

  implicit val BoolDecoder: Decoder[Boolean] = {
    case JBool(bool) => bool
  }

  implicit val BigDecimalDecoder: Decoder[BigDecimal] = {
    case JNumber(number) => number
  }

  implicit def stringToJValue(str: String) = JString(str)
  implicit def bigDecimalToJValue(b: BigDecimal) = JNumber(b)
  implicit def intToJValue(i: Int) = JNumber(BigDecimal(i))
  implicit def longToJValue(l: Long) = JNumber(BigDecimal(l))
  implicit def doubleToJValue(d: Double) = JNumber(BigDecimal(d))
  implicit def floatToJValue(f: Float) = JNumber(BigDecimal(f))
  implicit def booleanToJValue(b: Boolean) = JBool(b)

  implicit def jvalueTo[T](value: JValue)(implicit extract: Decoder[T], tag: TypeTag[T]): T =
    value.as[T](extract, tag)

}
