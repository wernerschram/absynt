package assembler.output.raw

import assembler._
import assembler.reference.{AbsoluteReference, SinglePassRelativeReference}
import assembler.sections.{LastIteration, Section}

class Raw[OffsetType<:Offset](section: Section, override val startOffset: Int)
  (implicit offsetFactory: OffsetFactory[OffsetType])
  extends Application(section :: Nil) {

  override def sectionOffset(section: Section with LastIteration): Long = startOffset

  override def encodeByte: List[Byte] = encodableSections.head.encodeByte

  override def intermediateResources(from: Reference): (List[Resource], OffsetDirection) = from match {
    case relative: SinglePassRelativeReference =>
      (section.intermediateEncodables(relative), section.offsetDirection(relative))
    case absolute: AbsoluteReference =>
      (section.content.takeWhile(r => r.label != absolute.target), OffsetDirection.Absolute)
  }
}

object Raw {
//  def apply[OffsetType](section: Section[OffsetType]) = new Raw(section, 0x100)
  def apply[OffsetType<:Offset](section: Section, startOffset: Int)
    (implicit offsetFactory: OffsetFactory[OffsetType]) =
      new Raw[OffsetType](section, startOffset)
}
