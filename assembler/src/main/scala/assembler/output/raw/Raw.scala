package assembler.output.raw

import assembler.{Application, Label, Resource}
import assembler.sections.{LastIteration, Section}

class Raw(section: Section, val baseAddress: Int) extends Application(section :: Nil) {

  def encodableSection: Section with LastIteration = section.encodable(this)


  override def memoryAddress(section: Section): Long = baseAddress

  override def encodeByte: List[Byte] = encodableSection.encodeByte
}

object Raw {
  def apply(section: Section) = new Raw(section, 0x100)
  def apply(section: Section, baseAddress: Int) = new Raw(section, baseAddress)
}
