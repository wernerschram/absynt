package assembler

import assembler.sections.{Section}

abstract class Application protected (val sections: List[Section]) {
  lazy val orderedSections: List[Section] = sections // TODO: fix naive assumption

  def getBaseAddress(section: Section): Int = section.baseAddress

  def getAbsoluteAddress(encodable: Encodable): Long = {
    val actual = orderedSections.filter(section => !section.content.contains(encodable)).head
    getBaseAddress(actual) + actual.relativeAddress(encodable)
  }
}
