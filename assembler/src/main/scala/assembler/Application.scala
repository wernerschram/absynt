package assembler

import assembler.sections.{LastIteration, Section}

abstract class Application protected (val sections: List[Section]) {
  lazy val encodableSections: List[Section with LastIteration] = sections.map(_.encodable(this))

  def getAbsoluteAddress(encodable: Resource): Long =
    encodableSections.filter(s=> s.contains(encodable))
      .map(s => memoryAddress(s) + s.relativeAddress(encodable)).head

  def getAbsoluteAddress(label: Label): Long =
    encodableSections.filter(s => s.contains(label))
      .map(s => memoryAddress(s) + s.relativeAddress(label)).head

  def memoryAddress(section: Section): Long

  def getAbsoluteMinimumAddress(label: Label): Long =
    sections.filter(s => s.contains(label))
      .map(s => memoryAddress(s) + s.minimumRelativeAddress(label)).head

  def getAbsoluteMaximumAddress(label: Label): Long =
    sections.filter(s => s.contains(label))
      .map(s => memoryAddress(s) + s.maximumRelativeAddress(label)).head

  def encodeByte: List[Byte]
}
