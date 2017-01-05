package assembler

import assembler.sections.Section

abstract class Application protected (val sections: List[Section]) extends Section {
}

class ARMBootSector(val initialSection: Section) extends Application (initialSection :: Nil) {

  def encodeByte: Seq[Byte] = initialSection.encodeByte()

  override val content: Seq[Encodable] = initialSection.content

  override def encodableLocation(encodable: Encodable): Int = initialSection.encodableLocation(encodable)

  override def getEncodableByLabel(label: Label): Encodable = initialSection.getEncodableByLabel(label)

  override def intermediateEncodables(from: Int, to: Int): Seq[Encodable] = initialSection.intermediateEncodables(from, to)
}