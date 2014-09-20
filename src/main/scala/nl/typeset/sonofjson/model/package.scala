package nl.typeset.sonofjson

import java.io.Reader

import scala.collection.mutable
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.Position
import scala.reflect.runtime.universe._
import scala.language.dynamics

package object model {

  type Extractor[T] = PartialFunction[JValue, T]
  implicit val IntExtractor: Extractor[Int] = { case JNumber(number) => number.toInt }
  implicit val DoubleExtractor: Extractor[Double] = { case JNumber(number) => number.toDouble }
  implicit val FloatExtractor: Extractor[Float] = { case JNumber(number) => number.toFloat }
  implicit val LongExtractor: Extractor[Long] = { case JNumber(number) => number.toLong }
  implicit val StringExtractor: Extractor[String] = {
    case JString(str) => str
    case JNumber(number) => number.toString()
    case JNull => "null"
    case JUndefined => "#undefined"
    case JBool(bool) => bool.toString
  }
  implicit val BoolExtractor: Extractor[Boolean] = { case JBool(bool) => bool }
  implicit val BigDecimalExtractor: Extractor[BigDecimal] = { case JNumber(number) => number }
  implicit def jvalueTo[T](value: JValue)(implicit extract: Extractor[T], tag: TypeTag[T]): T =
    value.as[T](extract, tag)

  sealed abstract class JValue extends Dynamic {

    def selectDynamic(name: String): JValue = this match {
      case JObject(elements) => elements.getOrElse(name, JUndefined)
      case _ => throw NotSupportedException(s"""$toString does not have an attribute called "$name"""")
    }

    def as[T](implicit extract: Extractor[T], tag: TypeTag[T]) =
      extract.applyOrElse(this, { _: JValue =>
        throw NotSupportedException(s"Missing support for extracting ${tag.tpe.typeSymbol.asClass.fullName}} from $this")
      })

  }
  case class JObject(elements: mutable.Map[String, JValue]) extends JValue
  case class JArray(elements: mutable.Buffer[JValue]) extends JValue
  case class JNumber(value: BigDecimal) extends JValue
  case class JBool(value: Boolean) extends JValue
  case class JString(value: String) extends JValue
  case object JNull extends JValue
  case object JUndefined extends JValue

  def parse(str: String) = Parser.parse(str)
  def parse(reader: Reader) = Parser.parse(reader)

  object Parser extends JavaTokenParsers {

    def stripQuotes(str: String) =
      str.substring(1, str.length - 1)

    def string: Parser[JString] = stringLiteral ^^ {
      str => JString(stripQuotes(str))
    }
    def number: Parser[JNumber] = floatingPointNumber ^^ { str => JNumber(BigDecimal(str)) }
    def bool: Parser[JBool] = (literal("true") | literal("false")) ^^ { str => JBool(str.toBoolean) }
    def `null`: Parser[JNull.type] = literal("null") ^^ { _ => JNull }
    def value: Parser[JValue] = obj | arr | string | number | bool | `null`
    def arr: Parser[JArray] = "[" ~> repsep(value, ",") <~ "]" ^^ { values => JArray(values.toBuffer) }
    def fld: Parser[(String, JValue)] = stringLiteral ~ (":" ~> value) ^^ { fld => stripQuotes(fld._1) -> fld._2 }
    def obj: Parser[JObject] = "{" ~> repsep(fld, ",") <~ "}" ^^ {
      flds => JObject(mutable.Map(flds.toSeq: _*))
    }

    def handle[T](result: ParseResult[JValue]): JValue = {
      result match {
        case Success(result, _) => result
        case NoSuccess(msg, next) => throw new ParserException(
          s"Failed to parse JSON: $msg at ${next.pos.toString()}\n${next.pos.longString}",
          next.pos
        )
      }
    }
    
    def parse(str: String) = handle(parseAll(value, str))
    
    def parse(reader: Reader) = handle(parseAll(value, reader))

  }

  case class ParserException(msg: String, pos: Position) extends Exception(msg)
  case class NotSupportedException(msg: String) extends Exception(msg)


}
