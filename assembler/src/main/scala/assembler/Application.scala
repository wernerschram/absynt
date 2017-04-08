package assembler

import assembler.reference.ReferencingInstruction
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

class ARMBootSector(val initialSection: Section)(implicit val label: Label) extends Application (initialSection :: Nil) {

  override def encodeByte: List[Byte] = initialSection.encodeByte

  override val content: List[Designation[Encodable]] = initialSection.content

  override def intermediateEncodables(from: ReferencingInstruction): List[Encodable] = initialSection.intermediateEncodables(from)

  override def isForwardReference(from: ReferencingInstruction): Boolean = initialSection.isForwardReference(from)

  override def size: Int = initialSection.size
}
