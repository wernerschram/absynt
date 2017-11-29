package assembler.output.raw

import assembler._
import assembler.reference.{AbsoluteReference, SinglePassRelativeReference}
import assembler.sections.{LastIteration, Section}

class Raw(section: Section, override val startOffset: Int)
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
  def apply(section: Section, startOffset: Int) = new Raw(section, startOffset)
}
