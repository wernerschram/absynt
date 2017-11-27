package assembler.output.raw

import assembler._
import assembler.reference.{AbsoluteReference, SinglePassRelativeReference}
import assembler.sections.{LastIteration, Section}

class Raw[OffsetType<:Offset, AddressType<:Address[OffsetType]](section: Section[OffsetType], override val startOffset: Int)
  (implicit offsetFactory: OffsetFactory[OffsetType], addressFactory: AddressFactory[OffsetType, AddressType])
  extends Application[OffsetType](section :: Nil) {

  override def sectionOffset(section: Section[OffsetType] with LastIteration[OffsetType]): Long = startOffset

  override def encodeByte: List[Byte] = encodableSections.head.encodeByte

  override def intermediateResources(from: Reference) = from match {
    case relative: SinglePassRelativeReference[OffsetType] =>
      (section.intermediateEncodables(relative), section.offsetDirection(relative))
    case absolute: AbsoluteReference[OffsetType, AddressType] =>
      (section.content.takeWhile(r => r.label != absolute.target), OffsetDirection.Absolute)
  }
}

object Raw {
//  def apply[OffsetType](section: Section[OffsetType]) = new Raw(section, 0x100)
  def apply[OffsetType<:Offset, AddressType<:Address[OffsetType]](section: Section[OffsetType], startOffset: Int)
    (implicit offsetFactory: OffsetFactory[OffsetType], addressFactory: AddressFactory[OffsetType, AddressType]) =
      new Raw[OffsetType, AddressType](section, startOffset)
}
