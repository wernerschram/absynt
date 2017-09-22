package assembler

import assembler.sections.{LastIteration, Section}

abstract class Application protected (val sections: List[Section]) {
  lazy val encodableSections: List[Section with LastIteration] = sections.map(_.encodable(this))

  def getAbsoluteAddress(encodable: Resource): Long

  def encodeByte: List[Byte]
}
