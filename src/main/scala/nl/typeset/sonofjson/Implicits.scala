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

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._

trait Implicits {

  implicit def cbf[V] = new CanBuildFrom[Seq[JValue], JValue, JArray] {
    private def newBuilder = new mutable.Builder[JValue, JArray] {
      private val buffer = new ArrayBuffer[JValue]

      override def +=(elem: JValue): this.type = {
        buffer += elem
        this
      }

      override def result(): JArray = JArray(buffer)

      override def clear(): Unit = buffer.clear()
    }

    override def apply(from: Seq[JValue]): mutable.Builder[JValue, JArray] = newBuilder

    override def apply(): mutable.Builder[JValue, JArray] = newBuilder
  }

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

  implicit def ListDecoder[T](implicit decoder: Decoder[T]): Decoder[List[T]] = {
    case JArray(elements) => elements.toList.map(decoder)
  }

  implicit def stringToJValue(str: String) = JString(str)
  implicit def bigDecimalToJValue(b: BigDecimal) = JNumber(b)
  implicit def intToJValue(i: Int) = JNumber(BigDecimal(i))
  implicit def longToJValue(l: Long) = JNumber(BigDecimal(l))
  implicit def doubleToJValue(d: Double) = JNumber(BigDecimal(d))
  implicit def floatToJValue(f: Float) = JNumber(BigDecimal(f))
  implicit def booleanToJValue(b: Boolean) = JBool(b)
  implicit def listToJValue[T](lst: List[T]) =
    JArray(lst.toBuffer.map(anyToJValue))
  implicit def mapToJValue[T](map: Map[String, T]) =
    JObject(mutable.Map(
      (for ((key, value) <- map.toSeq) yield {
        key -> anyToJValue(value)
      }): _*
    ))
  def anyToJValue[T](value: Any): JValue = value match {
    case str: String => stringToJValue(str)
    case b: BigDecimal => bigDecimalToJValue(b)
    case i: Int => intToJValue(i)
    case l: Long => longToJValue(l)
    case d: Double => doubleToJValue(d)
    case f: Float => floatToJValue(f)
    case b: Boolean => booleanToJValue(b)
    case l: List[_] => listToJValue(l)
    case m: Map[_, _] => mapToJValue(m.asInstanceOf[Map[String, _]])
  }

  implicit def jvalueTo[T](value: JValue)(implicit extract: Decoder[T], tag: TypeTag[T]): T =
    value.as[T](extract, tag)

}
