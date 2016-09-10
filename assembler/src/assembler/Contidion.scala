package assembler

abstract class Condition {
  def filterList[T](list: List[T]): List[T]
}

class LabelCondition(labelMatcher: LabeledEncodable => Boolean) extends Condition {
  def filterList[T](list: List[T]) =
    list.filter {
      case x: LabeledEncodable => labelMatcher(x);
      case default => false
    }
}

object StringLabelCondition {
  def apply(text: String) = new LabelCondition(x => x.label match { case label: StringLabel => label.value == text }) {
    override def toString() = text
  }
}