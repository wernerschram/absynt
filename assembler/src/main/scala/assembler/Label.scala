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

abstract class Designation[TargetType >: Encodable] {
  def label: Label
  def target: TargetType
  def isLabeled: Boolean
}

case class Labeled[TargetType >: Encodable](override val label: Label, override val target: TargetType) extends Designation[TargetType] {
  override def isLabeled: Boolean = true
}

case class Unlabeled[TargetType >: Encodable](t: TargetType) extends Designation[TargetType] {
  override def label: Nothing =
    throw new NoSuchElementException("head of empty labeled list")

  val target: TargetType = t

  override def isLabeled: Boolean = false
}
