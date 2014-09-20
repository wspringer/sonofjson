package nl.typeset.sonofjson.model

import scala.reflect.runtime.universe._

trait Implicits {

  implicit val IntExtractor: Extractor[Int] = {
    case JNumber(number) => number.toInt
  }

  implicit val DoubleExtractor: Extractor[Double] = {
    case JNumber(number) => number.toDouble
  }

  implicit val FloatExtractor: Extractor[Float] = {
    case JNumber(number) => number.toFloat
  }

  implicit val LongExtractor: Extractor[Long] = {
    case JNumber(number) => number.toLong
  }

  implicit val StringExtractor: Extractor[String] = {
    case JString(str) => str
    case JNumber(number) => number.toString()
    case JNull => "null"
    case JUndefined => "#undefined"
    case JBool(bool) => bool.toString
  }

  implicit val BoolExtractor: Extractor[Boolean] = {
    case JBool(bool) => bool
  }

  implicit val BigDecimalExtractor: Extractor[BigDecimal] = {
    case JNumber(number) => number
  }

  implicit def jvalueTo[T](value: JValue)(implicit extract: Extractor[T], tag: TypeTag[T]): T =
    value.as[T](extract, tag)

}
