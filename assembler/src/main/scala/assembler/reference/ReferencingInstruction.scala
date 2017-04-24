package assembler.reference

import assembler.{Encodable, Label}
import assembler.sections.Section

import scala.collection.concurrent.TrieMap

trait ReferencingInstruction
    extends Encodable {
  def target: Label

  def minimumSize: Int
  def maximumSize: Int

  def getSizeForDistance(forward: Boolean, distance: Int)(implicit page: Section): Int

  def encodeForDistance(forward: Boolean, distance: Int)(implicit page: Section): List[Byte]

  val pageMap = new TrieMap[Section, ReferencingInstructionOnPage]

  private def getOrElseCreateInstruction()(implicit page: Section): ReferencingInstructionOnPage = {
    pageMap.getOrElseUpdate(page, new ReferencingInstructionOnPage(this, target)(page))
  }

  def minimumEstimatedSize()(implicit page: Section): Int = getOrElseCreateInstruction.minimumEstimatedSize
  def maximumEstimatedSize()(implicit page: Section): Int = getOrElseCreateInstruction.maximumEstimatedSize

  def isEstimated()(implicit page: Section): Boolean = getOrElseCreateInstruction.isEstimated

  def estimatedSize(sizeAssumptions: Map[ReferencingInstruction, Int])(implicit page: Section): Int =
    getOrElseCreateInstruction.estimateSize(sizeAssumptions)

  override def size()(implicit page: Section): Int = getOrElseCreateInstruction().size

  override def encodeByte()(implicit page: Section): List[Byte] = getOrElseCreateInstruction.encodeByte
}
