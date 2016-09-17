package assembler

import scala.language.implicitConversions

abstract class Condition {
  def filter[T](list: Seq[T]): Seq[T]
}

object Condition {
  implicit def apply(text: String) = new LabelCondition(x => x.label match { case label: StringLabel => label.value == text }) {
    override def toString() = text
  }
}

class LabelCondition(labelMatcher: LabeledEncodable => Boolean) extends Condition {
  override def filter[T](list: Seq[T]) =
    list.filter {
      case x: LabeledEncodable => labelMatcher(x);
      case default => false
    }
}