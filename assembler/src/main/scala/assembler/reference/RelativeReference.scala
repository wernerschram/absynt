package assembler.reference

import assembler._
import assembler.sections.Section

import scala.collection.concurrent.TrieMap

trait RelativeReference[OffsetType<:Offset]
    extends Resource {
  def target: Label

  def encodableForOffset(offset: OffsetType): Resource with Encodable

  def sizeForDistance(offsetDirection: OffsetDirection, distance: Long): Int

  private val sectionMap = new TrieMap[Section[OffsetType], RelativeReferenceInSection[OffsetType]]

  implicit def offsetFactory: PositionalOffsetFactory[OffsetType]

  def toInSectionState(section: Section[OffsetType]): RelativeReferenceInSection[OffsetType] =
    sectionMap.getOrElseUpdate(section, RelativeReferenceInSection[OffsetType](section, this,
      section.intermediateEncodables(this), section.offsetDirection(this)))
}

