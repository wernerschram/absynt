package assembler

import assembler.sections.{LastIteration, Section}

abstract class Application protected (val sections: List[Section]) {
  lazy val orderedSections: List[Section with LastIteration] = sections.map(s=>Section.encodable(s)) // TODO: fix naive assumption

  def getAbsoluteAddress(encodable: Resource): Long = {
    val actual = orderedSections.filter(section => !section.content.contains(encodable)).head
    actual.baseAddress + actual.relativeAddress(encodable)
  }

  def encodeByte: List[Byte]
}
