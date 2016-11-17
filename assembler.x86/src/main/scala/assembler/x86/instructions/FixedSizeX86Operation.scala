package assembler.x86.instructions

import assembler.Label
import assembler.LabeledEncodable
import assembler.memory.MemoryPage
import assembler.x86.RexExtendedRequirement
import assembler.x86.operands.SegmentRegister
import assembler.x86.operations.Operation
import assembler.x86.ProcessorMode

trait FixedSizeX86Operation extends X86Operation {
  override def size()(implicit page: MemoryPage) = encodeByte().length
  override def withLabel(label: Label): LabeledEncodable = new LabeledX86Instruction(this, label)
}

trait FixedSizeX86Operation2 extends X86Operation {
  override def size()(implicit page: MemoryPage) = encodeByte().length
  override def withLabel(label: Label): LabeledEncodable = new LabeledX86Instruction(this, label)

  implicit val processorMode: ProcessorMode
  val includeRexW: Boolean = true
  val code: List[Byte]

  def operandSize: Option[Int]
  def addressSize: Option[Int]
  def segmentOverride: Option[SegmentRegister]
  def rexRequirements: List[RexExtendedRequirement]

  override def encodeByte()(implicit page: MemoryPage): List[Byte] = {

    Operation.optionalSegmentOverridePrefix(segmentOverride) :::
      Operation.optionalOperandSizePrefix(operandSize) :::
      Operation.optionalAddressSizePrefix(addressSize) :::
      Operation.optionalRexPrefix(operandSize, rexRequirements, includeRexW) :::
      code
  }
}

class LabeledX86Instruction(instruction: X86Operation, override val label: Label) extends X86Operation with LabeledEncodable {
  override def size()(implicit page: MemoryPage) = instruction.size()
  override def encodeByte()(implicit page: MemoryPage): List[Byte] = instruction.encodeByte()

  override def withLabel(label: Label): LabeledEncodable = new LabeledX86Instruction(this, label)

  override def toString() = s"${label.toString}: ${instruction.toString()}"
}