package assembler

import assembler.sections.Section

abstract class Application protected (val sections: List[Section]) extends Section {
}

class ARMBootSector(val initialSection: Section) extends Application (initialSection :: Nil) {

  def encodeByte: Seq[Byte] = initialSection.encodeByte()

  override val content: Seq[Encodable] = initialSection.content

  override def intermediateEncodables(from: Encodable, to: Label)  = initialSection.intermediateEncodables(from, to)

  override def isForwardReference(from: Encodable, to: Label) = initialSection.isForwardReference(from, to)
}