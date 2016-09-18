package assembler.reference

import assembler.Encodable
import assembler.Label
import assembler.LabeledEncodable
import assembler.memory.MemoryPage

trait ReferencingInstruction[T <: ReferencingInstructionOnPage]
    extends Encodable {
  def getOrElseCreateInstruction()(implicit page: MemoryPage): T

  def minimumEstimatedSize()(implicit page: MemoryPage): Int = getOrElseCreateInstruction.minimumEstimatedSize
  def maximumEstimatedSize()(implicit page: MemoryPage): Int = getOrElseCreateInstruction.maximumEstimatedSize

  def isEstimated()(implicit page: MemoryPage): Boolean = getOrElseCreateInstruction.isEstimated

  def estimatedSize(sizeAssumptions: Map[ReferencingInstructionOnPage, Int])(implicit page: MemoryPage): Int =
    getOrElseCreateInstruction.estimatedSize(sizeAssumptions)

//  def size()(implicit page: MemoryPage) = getOrElseCreateInstruction.size
  override def encodeByte()(implicit page: MemoryPage): List[Byte] = getOrElseCreateInstruction.encodeByte

  override def withLabel(label: Label): LabeledEncodable = new LabeledReferencingInstruction[T](this, label)
}


class LabeledReferencingInstruction[T <: ReferencingInstructionOnPage](
    instruction: ReferencingInstruction[T],
    val label: Label) extends ReferencingInstruction[T] with LabeledEncodable {
  override def getOrElseCreateInstruction()(implicit page: MemoryPage): T = instruction.getOrElseCreateInstruction()

  override def size()(implicit page: MemoryPage): Int = instruction.size()
  override def encodeByte()(implicit page: MemoryPage): List[Byte] = instruction.encodeByte()

  override def withLabel(label: Label): LabeledEncodable = new LabeledReferencingInstruction[T](this, label)
}