package assembler

import scala.language.implicitConversions

class Label

object Label {
  implicit def apply(value: String): Label = StringLabel(value)

  def unique = {
    lastId += 1
    UniqueLabel(lastId)
  }
  
  private var lastId = 0
}

case class StringLabel private (val value: String) extends Label {
  override def toString(): String = value
}

case class UniqueLabel private (val id: Int) extends Label {
  
}

trait Labeled {
  def label: Label
}
