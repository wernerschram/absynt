package assembler.reference

import assembler.{Encodable, Label}
import assembler.sections.Section

import scala.collection.concurrent.TrieMap

trait ReferencingInstruction
    extends Encodable {
  def target: Label

  def minimumSize: Int
  def maximumSize: Int

  def encodableForDistance(forward: Boolean, distance: Int)(implicit page: Section): Encodable

  def getSizeForDistance(forward: Boolean, distance: Int)(implicit page: Section): Int =
    encodableForDistance(forward, distance).size

  def encodeForDistance(forward: Boolean, distance: Int)(implicit page: Section): Seq[Byte] =
    encodableForDistance(forward, distance).encodeByte

  private val pageMap = new TrieMap[Section, ReferencingInstructionOnPage]

  private def getOrElseCreateInstruction()(implicit page: Section): ReferencingInstructionOnPage =
    pageMap.getOrElseUpdate(page, new ReferencingInstructionOnPage(this, target)(page))

  final private[reference] def minimumEstimatedSize()(implicit page: Section): Int = getOrElseCreateInstruction.minimumEstimatedSize
  final private[reference] def maximumEstimatedSize()(implicit page: Section): Int = getOrElseCreateInstruction.maximumEstimatedSize

  final private[reference] def isEstimated()(implicit page: Section): Boolean = getOrElseCreateInstruction.isEstimated

  final private[reference] def estimatedSize(sizeAssumptions: Map[ReferencingInstruction, Int])(implicit page: Section): Int =
    getOrElseCreateInstruction.estimateSize(sizeAssumptions)

  override def size()(implicit page: Section): Int = getOrElseCreateInstruction().size

  override def encodeByte()(implicit page: Section): Seq[Byte] = getOrElseCreateInstruction.encodeByte
}
