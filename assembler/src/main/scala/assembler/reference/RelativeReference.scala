package assembler.reference

import assembler._
import assembler.sections.Section

import scala.collection.concurrent.TrieMap

trait RelativeReference[OffsetType<:Offset]
    extends Reference {

  def encodableForOffset(offset: OffsetType): Resource with Encodable

  def sizeForDistance(offsetDirection: OffsetDirection, distance: Long): Int

  private val sectionMap = new TrieMap[Section[OffsetType], BoundRelativeReference[OffsetType]]

  final def estimateSize(
    assumption: Int, sizeAssumptions: Map[RelativeReference[OffsetType], Int])
    (section: Section[OffsetType]): Int =
      bind(section).estimateSize(assumption, sizeAssumptions)

  final def size(section: Section[OffsetType]): Int =
    bind(section).size

  implicit def offsetFactory: PositionalOffsetFactory[OffsetType]

  def bind(section: Section[OffsetType]): BoundRelativeReference[OffsetType] =
    sectionMap.getOrElseUpdate(section, BoundRelativeReference[OffsetType](section, this))
}
