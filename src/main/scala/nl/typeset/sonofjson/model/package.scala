package nl.typeset.sonofjson

import java.io.Reader

import scala.collection.mutable
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.Position

package object model {

  sealed trait JValue
  case class JObject(elements: mutable.Map[String, JValue]) extends JValue
  case class JArray(elements: mutable.Buffer[JValue]) extends JValue
  case class JNumber(value: BigDecimal) extends JValue
  case class JBool(value: Boolean) extends JValue
  case class JString(value: String) extends JValue
  case object JNull extends JValue

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

}
