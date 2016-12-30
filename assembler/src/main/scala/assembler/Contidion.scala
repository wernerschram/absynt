//package assembler
//
//import scala.language.implicitConversions
//
//abstract class Condition {
//  def filter[T](list: Seq[T]): Seq[T]
//}
//
//object Condition {
//  implicit def apply(text: String): LabelCondition =
//    new LabelCondition(x => x.label match { case label: StringLabel => label.value == text }) {
//      override def toString() = text
//    }
//  
//  implicit def apply(label: Label): LabelCondition = 
//    new LabelCondition(x => x.label == label) {
//      override def toString() = label.toString()
//    }
//}
//
//class LabelCondition(labelMatcher: LabeledEncodable => Boolean) extends Condition {
//  override def filter[T](list: Seq[T]): Seq[T] =
//    list.filter {
//      case encodable: LabeledEncodable => labelMatcher(encodable);
//      case _ => false
//    }
//}
