package assembler.reference

import assembler.Encodable
import assembler.MemoryPage
import assembler.Label
import assembler.LabeledEncodable

trait ReferencingInstruction[T <: ReferencingInstructionOnPage]
    extends Encodable {
  def getOrElseCreateInstruction()(implicit page: MemoryPage): T

  def minimumEstimatedSize()(implicit page: MemoryPage) = getOrElseCreateInstruction.minimumEstimatedSize
  def maximumEstimatedSize()(implicit page: MemoryPage) = getOrElseCreateInstruction.maximumEstimatedSize

  def sizeIsKnown()(implicit page: MemoryPage) = getOrElseCreateInstruction.sizeIsKnown

  def estimatedSize(sizeAssumptions: Map[ReferencingInstructionOnPage, Int])(implicit page: MemoryPage) = getOrElseCreateInstruction.estimatedSize(sizeAssumptions)

//  def size()(implicit page: MemoryPage) = getOrElseCreateInstruction.size
  override def encodeByte()(implicit page: MemoryPage) = getOrElseCreateInstruction.encodeByte
  
  override def withLabel(label: Label) = new LabeledReferencingInstruction[T](this, label)
}


class LabeledReferencingInstruction[T <: ReferencingInstructionOnPage](
    instruction: ReferencingInstruction[T],
    val label: Label) extends ReferencingInstruction[T] with LabeledEncodable {
  override def getOrElseCreateInstruction()(implicit page: MemoryPage): T = instruction.getOrElseCreateInstruction()

  override def size()(implicit page: MemoryPage) = instruction.size()
  override def encodeByte()(implicit page: MemoryPage): List[Byte] = instruction.encodeByte()
  
  override def withLabel(label: Label) = new LabeledReferencingInstruction[T](this, label)
}
