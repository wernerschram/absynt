package assembler.output.raw

import assembler.{Application, Resource}
import assembler.sections.Section

class Raw(section: Section) extends Application(section :: Nil) {

  def encodableSection = section.encodable(this)

  val baseAddress = 0x100

  override def encodeByte = encodableSection.encodeByte

  override def getAbsoluteAddress(encodable: Resource) = encodableSection.relativeAddress(encodable) + baseAddress
}

object Raw {
  def apply(section: Section) = new Raw(section)
}
