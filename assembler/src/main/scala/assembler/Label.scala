package assembler

import scala.language.implicitConversions

abstract class Label {
  def matches(label: Label): Boolean
}

object Label {
  implicit val label = noLabel

  implicit def apply(value: String): Label = StringLabel(value)

  def unique: UniqueLabel = synchronized {
    lastId += 1
    UniqueLabel(lastId)
  }

  def noLabel: Label = NoLabel()

  private var lastId = 0
}

case class NoLabel private() extends Label {
  def matches(label: Label): Boolean = false
}

case class StringLabel private (value: String) extends Label {
  override def toString: String = value

  def matches(label: Label): Boolean = this.equals(label)
}

case class UniqueLabel private (id: Int) extends Label {
  def matches(label: Label): Boolean = this.equals(label)
}
