package nl.typeset

import org.json4s.JsonAST._
import org.json4s.native.JsonMethods
import org.json4s.{BigDecimalMode, Implicits}
import scala.language.dynamics

package object sonofjson {

  def parse(str: String) = Json(JsonMethods.parse(str))

  object obj extends Dynamic with Implicits with BigDecimalMode {

    def applyDynamicNamed(method: String)(args: (String, Any)*) = method match {
      case "apply" =>
        Json(JObject(for ((name, value) <- args.toList) yield JField(name, value match {
          case i: Int => JInt(i)
          case i: Long => JInt(i)
          case d: Double => JDouble(d)
          case d: Float => JDouble(d)
          case s: String => JString(s)
          case b: Boolean => JBool(b)
          case obj: Json[_] => obj.value
          case other => throw NotSupportedException("Missing support for " + other.getClass)
        })))
    }

  }

  object arr extends Dynamic with Implicits with BigDecimalMode {
    def applyDynamic(method: String)(args: Any*) = method match {
      case "apply" =>
        Json(JArray(for (arg <- args.toList) yield arg match {
          case i: Int => JInt(i)
          case i: Long => JInt(i)
          case d: Double => JDouble(d)
          case d: Float => JDouble(d)
          case s: String => JString(s)
          case b: Boolean => JBool(b)
          case obj: Json[_] => obj.value
          case other => throw NotSupportedException("Missing support for " + other.getClass)
        }))
    }
  }

}
