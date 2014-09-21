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

import java.io.Reader

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.Position
import scala.reflect.runtime.universe._
import scala.language.dynamics

package object model extends Implicits {

  type Decoder[T] = PartialFunction[JValue, T]

  sealed abstract class JValue extends Dynamic {

    def selectDynamic(name: String): JValue = this match {
      case JObject(elements) => elements.get(name) match {
        case Some(value) => value
        case None => throw NotSupportedException( s"""Missing key "$name" for $this""")
      }
      case _ => throw NotSupportedException( s"""$toString does not have an attribute called "$name"""")
    }

    def as[T](implicit decoder: Decoder[T], tag: TypeTag[T]) =
      decoder.applyOrElse(this, { _: JValue =>
        throw NotSupportedException(s"Missing support for extracting ${tag.tpe.typeSymbol.asClass.fullName}} from $this")
      })

    def apply(index: Int) = this match {
      case JArray(elements) => elements(index)
      case _ => throw NotSupportedException(s"Missing support for extracting elements by position from $this")
    }

    def applyDynamic(name: String)(index: Int) = {
      val named = selectDynamic(name)
      named.apply(index)
    }

    def updateDynamic(name: String)(value: JValue) = this match {
      case JObject(elements) => elements += name -> value
      case _ => throw NotSupportedException(s"No support for setting attributes on $this")
    }

    def update(index: Int, value: JValue) = this match {
      case JArray(elements) => elements.update(index, value)
      case _ => throw NotSupportedException(s"$this is not something that positional updates")
    }

  }

  case class JObject(elements: mutable.Map[String, JValue]) extends JValue

  case class JArray(elements: mutable.Buffer[JValue]) extends JValue

  case class JNumber(value: BigDecimal) extends JValue

  case class JBool(value: Boolean) extends JValue

  case class JString(value: String) extends JValue

  case object JNull extends JValue

  def parse(str: String) = Parser.parse(str)

  def parse(reader: Reader) = Parser.parse(reader)

  def render(value: JValue, out: Appendable): Unit = value match {
    case JObject(elements) =>
      def field(key: String, value: JValue) = {
        out.append(s""""${escape(key)}":""")
        render(value, out)
      }
      @tailrec
      def fields(elements: List[(String, JValue)], prefix: String = "") {
        if (!elements.isEmpty) {
          val next = elements.head
          out.append(prefix)
          field(next._1, next._2)
          fields(elements.tail, ",")
        }
      }
      out.append("{")
      fields(elements.toList)
      out.append("}")
    case JArray(elements) =>
      @tailrec
      def allElements(elements: List[JValue], prefix: String = "") {
        if (!elements.isEmpty) {
          out.append(prefix)
          render(elements.head, out)
          allElements(elements.tail, ",")
        }
      }
      out.append("[")
      allElements(elements.toList)
      out.append("]")
    case JString(str) =>
      out.append(s""""${escape(str)}"""")
    case JNumber(number) =>
      out.append(number.toString())
    case JNull =>
      out.append("null")
    case JBool(bool) =>
      out.append(bool.toString)
  }

  def render(value: JValue): String = {
    val builder = new java.lang.StringBuilder()
    render(value, builder)
    builder.toString
  }

  object Parser extends JavaTokenParsers {

    def stripQuotes(str: String) =
      str.substring(1, str.length - 1)

    def string: Parser[JString] = stringLiteral ^^ {
      str => JString(stripQuotes(str))
    }

    def number: Parser[JNumber] = floatingPointNumber ^^ { str => JNumber(BigDecimal(str))}

    def bool: Parser[JBool] = (literal("true") | literal("false")) ^^ { str => JBool(str.toBoolean)}

    def `null`: Parser[JNull.type] = literal("null") ^^ { _ => JNull}

    def value: Parser[JValue] = obj | arr | string | number | bool | `null`

    def arr: Parser[JArray] = "[" ~> repsep(value, ",") <~ "]" ^^ { values => JArray(values.toBuffer)}

    def fld: Parser[(String, JValue)] = stringLiteral ~ (":" ~> value) ^^ { fld => stripQuotes(fld._1) -> fld._2}

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

  object obj extends Dynamic {

    def applyDynamicNamed(method: String)(args: (String, JValue)*) = method match {
      case "apply" =>
        JObject(mutable.Map[String, JValue](args: _*))
    }

  }

  object arr extends Dynamic {
    def applyDynamic(method: String)(args: JValue*) = method match {
      case "apply" =>
        JArray(mutable.Buffer(args: _*))
    }
  }

  // Need a faster and less hacky escaping solution
  private def escape(raw: String): String =
    Literal(Constant(raw)).toString.stripPrefix("\"").stripSuffix("\"")

}
