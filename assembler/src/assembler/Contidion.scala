package assembler

import scala.language.implicitConversions

abstract class Condition {
  def filterList[T](list: List[T]): List[T]
}

object Condition {
  implicit def apply(text: String) = new LabelCondition(x => x.label match { case label: StringLabel => label.value == text }) {
    override def toString() = text
  }
}

class LabelCondition(labelMatcher: LabeledEncodable => Boolean) extends Condition {
  def filterList[T](list: List[T]) =
    list.filter {
      case x: LabeledEncodable => labelMatcher(x);
      case default => false
    }
}