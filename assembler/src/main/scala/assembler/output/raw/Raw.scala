package assembler.output.raw

import assembler.{Application, Label, Resource}
import assembler.sections.{LastIteration, Section}

class Raw(section: Section) extends Application(section :: Nil) {

  def encodableSection: Section with LastIteration = section.encodable(this)

  val baseAddress = 0x100

  override def encodeByte: List[Byte] = encodableSection.encodeByte

  override def getAbsoluteAddress(encodable: Resource): Long = encodableSection.relativeAddress(encodable) + baseAddress
  override def getAbsoluteAddress(label: Label): Long = encodableSection.relativeAddress(label) + baseAddress
}

object Raw {
  def apply(section: Section) = new Raw(section)
}
