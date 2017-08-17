package assembler

import assembler.sections.Section

class EncodableCollection private(val label: Label, val encodables: List[Encodable]) extends Encodable {

  override def encodeByte()(implicit page: Section): List[Byte] = encodables.flatMap(_.encodeByte())

  override def size()(implicit page: Section): Int = encodables.map(_.size()).sum

  override def toString: String = s"""${super.toString}${encodables.map(_.toString).mkString("; ")}"""
}

object EncodableCollection {
  def apply(encodables: List[Encodable])(implicit label: Label): EncodableCollection = new EncodableCollection(label, encodables)
}