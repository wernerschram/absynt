package assembler.reference

import assembler.Encodable
import assembler.Label
import assembler.LabeledEncodable
import assembler.sections.Section

trait ReferencingInstruction
    extends Encodable {
  def getOrElseCreateInstruction()(implicit page: Section): ReferencingInstructionOnPage

  def minimumEstimatedSize()(implicit page: Section): Int = getOrElseCreateInstruction.minimumEstimatedSize
  def maximumEstimatedSize()(implicit page: Section): Int = getOrElseCreateInstruction.maximumEstimatedSize

  def isEstimated()(implicit page: Section): Boolean = getOrElseCreateInstruction.isEstimated

  def estimatedSize(sizeAssumptions: Map[ReferencingInstructionOnPage, Int])(implicit page: Section): Int =
    getOrElseCreateInstruction.estimateSize(sizeAssumptions)

  override def encodeByte()(implicit page: Section): List[Byte] = getOrElseCreateInstruction.encodeByte

  override def withLabel(label: Label): LabeledEncodable = new LabeledReferencingInstruction(this, label)
}


class LabeledReferencingInstruction (
    override val value: ReferencingInstruction,
    val label: Label) extends ReferencingInstruction with LabeledEncodable {
  override def getOrElseCreateInstruction()(implicit page: Section): ReferencingInstructionOnPage = value.getOrElseCreateInstruction()

  override def withLabel(label: Label): LabeledEncodable = new LabeledReferencingInstruction(this, label)
}
