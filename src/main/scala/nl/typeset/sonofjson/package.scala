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

    def asJson = {
      val builder = new java.lang.StringBuilder()
      append(builder)
      builder.toString
    }

    def append(out: Appendable)

  }

  case class JObject(elements: mutable.Map[String, JValue]) extends JValue {
    override def append(out: Appendable): Unit = {
      def render(key: String, value: JValue) = {
        out.append( s""""${escape(key)}\":""")
        value.append(out)
      }
      @tailrec
      def renderAll(elements: List[(String, JValue)], prefix: String = "") {
        if (!elements.isEmpty) {
          val next = elements.head
          out.append(prefix)
          render(next._1, next._2)
          renderAll(elements.tail, ",")
        }
      }
      out.append("{")
      renderAll(elements.toList)
      out.append("}")
    }
  }

  case class JArray(elements: mutable.Buffer[JValue]) extends JValue {
    override def append(out: Appendable): Unit = {
      @tailrec
      def renderAll(elements: List[JValue], prefix: String = "") {
        if (!elements.isEmpty) {
          out.append(prefix)
          elements.head.append(out)
          renderAll(elements.tail, ",")
        }
      }
      out.append("[")
      renderAll(elements.toList)
      out.append("]")
    }
  }

  case class JNumber(value: BigDecimal) extends JValue {
    override def append(out: Appendable): Unit = out.append(value.toString())
  }

  case class JBool(value: Boolean) extends JValue {
    override def append(out: Appendable): Unit = out.append(value.toString)
  }

  case class JString(value: String) extends JValue {
    override def append(out: Appendable): Unit =
      out.append( s""""${escape(value)}"""")
  }

  case object JNull extends JValue {
    override def append(out: Appendable): Unit = {
      out.append("null")
    }
  }

  def parse(str: String) = Parser.parse(str)

  def parse(reader: Reader) = Parser.parse(reader)

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
