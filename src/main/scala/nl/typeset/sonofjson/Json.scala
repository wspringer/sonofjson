package nl.typeset.sonofjson

import org.json4s._
import org.json4s.native._
import org.json4s.native.JsonMethods._

import scala.language.dynamics
import scala.reflect.Manifest

sealed abstract class Ref {
  def resolve(name: String) = Named(this, name) 
  def resolve(index: Int) = Positional(this, index)
  def path: Option[String]
}
sealed abstract class SubRef[T <: Ref] extends Ref {
  def parent: T
}
case object Identity extends Ref {
  def path = None
}
case class Positional[T <: Ref](val parent: T, index: Int) extends SubRef[T] {
  override val path = Some(parent.path.getOrElse("") + "[" + index + "]")
}
case class Named[T <: Ref](val parent: T, name: String) extends SubRef[T] {
  override val path = Some(parent.path.getOrElse(".") + name)
}

class Json(value: JValue, ref: Ref = Identity) extends Dynamic with DefaultFormats {

  implicit val formats = DefaultFormats

  def selectDynamic(name: String) = new Json(value \ name, ref.resolve(name))

  def apply(index: Int) = value match {
    case arr: JArray => new Json(arr(index), ref.resolve(index))
    case _ => new Json(JNothing, ref.resolve(index))
  }

  def applyDynamic(name: String)(index: Int) = {
    val named = selectDynamic(name)
    named.apply(index)
  }

  def as[T](implicit mf: Manifest[T]) = value.extract[T]

  def path = ref.path

  private def valueAsString = value match {
    case JNothing => "undefined"
    case JNull => "null"
    case JString(str) => "\"" + str + "\""
    case obj: JObject => compact(render(obj))
    case arr: JArray => compact(render(arr))
    case other: JValue => other.values.toString
  }


  override def toString = path match {
    case Some(p) => s"Json($p, $valueAsString)"
    case None => s"Json($valueAsString)"
  }

}

object Json {
  def parse(str: String) = Json(JsonMethods.parse(str))
  def apply(value: JValue) = new Json(value)
}

