package assembler.reference

import assembler.{Encodable, Label}
import assembler.sections.Section

import scala.collection.concurrent.TrieMap

trait ReferencingInstruction
    extends Encodable {
  def target: Label

  def minimumSize: Int
  def maximumSize: Int

  val pageMap = new TrieMap[Section, ReferencingInstructionOnPage]

  def getOrElseCreateInstruction()(implicit page: Section): ReferencingInstructionOnPage = {
    pageMap.getOrElseUpdate(page, createOperation(this, target, page))
  }

  def createOperation(thisOperation: ReferencingInstruction, destination: Label, section: Section):
  ReferencingInstructionOnPage =
    new ReferencingInstructionOnPage(thisOperation, destination)(section)

  def minimumEstimatedSize()(implicit page: Section): Int = getOrElseCreateInstruction.minimumEstimatedSize
  def maximumEstimatedSize()(implicit page: Section): Int = getOrElseCreateInstruction.maximumEstimatedSize

  def isEstimated()(implicit page: Section): Boolean = getOrElseCreateInstruction.isEstimated

  def estimatedSize(sizeAssumptions: Map[ReferencingInstructionOnPage, Int])(implicit page: Section): Int =
    getOrElseCreateInstruction.estimateSize(sizeAssumptions)

  def getSizeForDistance(forward: Boolean, distance: Int)(implicit page: Section): Int

  def encodeForDistance(forward: Boolean, distance: Int)(implicit page: Section): List[Byte]

  override def encodeByte()(implicit page: Section): List[Byte] = getOrElseCreateInstruction.encodeByte
}
