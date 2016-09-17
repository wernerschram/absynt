package assembler

import scala.language.implicitConversions

class Label

object Label {
  implicit def apply(value: String) = StringLabel(value)
}

case class StringLabel private (val value: String) extends Label {
  override def toString() = value
}

trait Labeled {
  def label: Label
}