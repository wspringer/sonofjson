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
