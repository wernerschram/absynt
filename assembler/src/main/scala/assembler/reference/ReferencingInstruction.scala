package assembler.reference

import assembler.Encodable
import assembler.Label
import assembler.LabeledEncodable
import assembler.memory.MemoryPage

trait ReferencingInstruction
    extends Encodable {
  def getOrElseCreateInstruction()(implicit page: MemoryPage): ReferencingInstructionOnPage

  def minimumEstimatedSize()(implicit page: MemoryPage): Int = getOrElseCreateInstruction.minimumEstimatedSize
  def maximumEstimatedSize()(implicit page: MemoryPage): Int = getOrElseCreateInstruction.maximumEstimatedSize

  def isEstimated()(implicit page: MemoryPage): Boolean = getOrElseCreateInstruction.isEstimated

  def estimatedSize(sizeAssumptions: Map[ReferencingInstructionOnPage, Int])(implicit page: MemoryPage): Int =
    getOrElseCreateInstruction.estimateSize(sizeAssumptions)

  override def encodeByte()(implicit page: MemoryPage): List[Byte] = getOrElseCreateInstruction.encodeByte

  override def withLabel(label: Label): LabeledEncodable = new LabeledReferencingInstruction(this, label)
}


class LabeledReferencingInstruction (
    override val value: ReferencingInstruction,
    val label: Label) extends ReferencingInstruction with LabeledEncodable {
  override def getOrElseCreateInstruction()(implicit page: MemoryPage): ReferencingInstructionOnPage = value.getOrElseCreateInstruction()

  override def withLabel(label: Label): LabeledEncodable = new LabeledReferencingInstruction(this, label)
}
