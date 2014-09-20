package nl.typeset.sonofjson

import scala.collection.mutable
import scala.util.parsing.combinator.JavaTokenParsers

package object model {

  sealed trait JValue
  case class JObject(elements: mutable.Map[String, JValue]) extends JValue
  case class JArray(elements: mutable.Buffer[JValue]) extends JValue
  case class JNumber(value: BigDecimal) extends JValue
  case class JBool(value: Boolean) extends JValue
  case class JString(value: String) extends JValue
  case object JNull extends JValue

  object Parser extends JavaTokenParsers {

    def string: Parser[JString] = stringLiteral ^^ JString.apply
    def number: Parser[JNumber] = floatingPointNumber ^^ { str => JNumber(BigDecimal(str)) }
    def bool: Parser[JBool] = (literal("true") | literal("false")) ^^ { str => JBool(str.toBoolean) }
    def `null`: Parser[JNull.type] = literal("null") ^^ { _ => JNull }
    def value: Parser[JValue] = obj | arr | string | number | bool | `null`
    def arr: Parser[JArray] = "[" ~> repsep(value, ",") <~ "]" ^^ { values => JArray(values.toBuffer) }
    def fld: Parser[(String, JValue)] = stringLiteral ~ (":" ~> value) ^^ { fld => fld._1 -> fld._2 }
    def obj: Parser[JObject] = "{" ~> repsep(fld, ",") <~ "}" ^^ {
      flds => JObject(mutable.Map(flds.toSeq: _*))
    }

  }

}
