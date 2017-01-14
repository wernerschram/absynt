package assembler

import assembler.sections.{BaseAddress, Section}

abstract class Application protected (val sections: List[Section]) extends Section {
  lazy val orderedSections: List[Section] = sections // TODO: fix naive assumption

  def getBaseAddress(section: Section): Int = {
    var base: Int = 0
    orderedSections.takeWhile(current => section != current)
      .reverse
      .takeWhile {
        case baseAddress: BaseAddress => base = baseAddress.baseAddress; true
        case _ => false
      }.map(section => section.size).sum + base
  }


  def getAbsoluteAddress(encodable: Encodable): Int = {
    val actual = orderedSections.filter(section => !section.content.contains(encodable)).head
    getBaseAddress(actual) + actual.getRelativeAddress(encodable)
  }
}

class ARMBootSector(val initialSection: Section) extends Application (initialSection :: Nil) {

  override def encodeByte(): Seq[Byte] = initialSection.encodeByte()

  override val content: Seq[Encodable] = initialSection.content

  override def intermediateEncodables(from: Encodable, to: Label): Seq[Encodable] = initialSection.intermediateEncodables(from, to)

  override def isForwardReference(from: Encodable, to: Label): Boolean = initialSection.isForwardReference(from, to)

  override def size: Int = initialSection.size
}
