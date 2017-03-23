package assembler

import scala.language.implicitConversions

class Label

object Label {
  implicit def apply(value: String): Label = StringLabel(value)

  def unique: UniqueLabel = {
    lastId += 1
    UniqueLabel(lastId)
  }
  
  private var lastId = 0
}

case class StringLabel private (value: String) extends Label {
  override def toString: String = value
}

case class UniqueLabel private (id: Int) extends Label

case class Labeled[TargetType >: Encodable](label: Label, target: TargetType)
