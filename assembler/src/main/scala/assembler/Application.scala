package assembler

import assembler.sections.{LastIteration, Section}

abstract class Application protected (val sections: List[Section]) {
  lazy val orderedSections: List[Section with LastIteration] = sections.map(s=>Section.encodable(s)) // TODO: fix naive assumption

  def getBaseAddress(section: Section): Int = section.baseAddress

  def getAbsoluteAddress(encodable: Resource): Long = {
    val actual = orderedSections.filter(section => !section.content.contains(encodable)).head
    getBaseAddress(actual) + actual.relativeAddress(encodable)
  }
}
