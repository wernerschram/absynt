package assembler.output.raw

import assembler.{Application, Label, Resource}
import assembler.sections.{LastIteration, Section}

class Raw(section: Section, val baseAddress: Int) extends Application(section :: Nil) {

  def encodableSection: Section with LastIteration = section.encodable(this)


  override def encodeByte: List[Byte] = encodableSection.encodeByte

  override def getAbsoluteAddress(encodable: Resource): Long = encodableSection.relativeAddress(encodable) + baseAddress
  override def getAbsoluteAddress(label: Label): Long = encodableSection.relativeAddress(label) + baseAddress

  override def getAbsoluteMinimumAddress(label: Label): Long = section.minimumRelativeAddress(label) + baseAddress

  override def getAbsoluteMaximumAddress(label: Label): Long = section.maximumRelativeAddress(label) + baseAddress
}

object Raw {
  def apply(section: Section) = new Raw(section, 0x100)
  def apply(section: Section, baseAddress: Int) = new Raw(section, baseAddress)
}
