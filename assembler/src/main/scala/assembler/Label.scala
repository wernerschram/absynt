package assembler

import scala.language.implicitConversions

class Label

object Label {
  implicit def apply(value: String): Label = StringLabel(value)
}

case class StringLabel private (val value: String) extends Label {
  override def toString(): String = value
}

trait Labeled {
  def label: Label
}
