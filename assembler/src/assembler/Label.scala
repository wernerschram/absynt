package assembler

class Label

case class StringLabel(val value: String) extends Label {
  override def toString() = value
}

trait Labeled {
  def label: Label
}