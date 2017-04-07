package assembler.reference

import assembler.{Encodable, Label}
import assembler.sections.Section

trait ReferencingInstruction
    extends Encodable {
  def target: Label

  def getOrElseCreateInstruction()(implicit page: Section): ReferencingInstructionOnPage

  def minimumEstimatedSize()(implicit page: Section): Int = getOrElseCreateInstruction.minimumEstimatedSize
  def maximumEstimatedSize()(implicit page: Section): Int = getOrElseCreateInstruction.maximumEstimatedSize

  def isEstimated()(implicit page: Section): Boolean = getOrElseCreateInstruction.isEstimated

  def estimatedSize(sizeAssumptions: Map[ReferencingInstructionOnPage, Int])(implicit page: Section): Int =
    getOrElseCreateInstruction.estimateSize(sizeAssumptions)

  override def encodeByte()(implicit page: Section): List[Byte] = getOrElseCreateInstruction.encodeByte
}
