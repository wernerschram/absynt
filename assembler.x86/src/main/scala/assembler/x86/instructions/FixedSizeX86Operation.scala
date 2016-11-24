package assembler.x86.instructions

import assembler.Label
import assembler.LabeledEncodable
import assembler.memory.MemoryPage
import assembler.x86.RexExtendedRequirement
import assembler.x86.operands.SegmentRegister
import assembler.x86.operations.Operation
import assembler.x86.ProcessorMode
import assembler.x86.operands.Operand

//trait FixedSizeX86Operation extends X86Operation {
//  override def size()(implicit page: MemoryPage) = encodeByte().length
//  override def withLabel(label: Label): LabeledEncodable = new LabeledX86Instruction(this, label)
//}

trait FixedSizeX86Operation extends X86Operation {
  def validate: Unit = Unit

  def operands: List[Operand]

  override def size()(implicit page: MemoryPage) = encodeByte().length
  override def withLabel(label: Label): LabeledEncodable = new LabeledX86Instruction(this, label)

  implicit val processorMode: ProcessorMode
  val includeRexW: Boolean = true
  def code: List[Byte]
  val mnemonic: String

  def operandSize: Option[Int] = None
  def addressSize: Option[Int] = None
  def segmentOverride: Option[SegmentRegister] = None
  def rexRequirements: List[RexExtendedRequirement] = Nil

  override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
    validate

    Operation.optionalSegmentOverridePrefix(segmentOverride) :::
      Operation.optionalAddressSizePrefix(addressSize) :::
      Operation.optionalOperandSizePrefix(operandSize) :::
      Operation.optionalRexPrefix(operandSize, rexRequirements, includeRexW) :::
      code
  }

  override def toString() = s"${mnemonic} ${operands.reverseMap { operand => operand.toString() }.mkString(", ")}"
}

class LabeledX86Instruction(instruction: X86Operation, override val label: Label) extends X86Operation with LabeledEncodable {
  override def size()(implicit page: MemoryPage) = instruction.size()
  override def encodeByte()(implicit page: MemoryPage): List[Byte] = instruction.encodeByte()

  override def withLabel(label: Label): LabeledEncodable = new LabeledX86Instruction(this, label)

  override def toString() = s"${label.toString}: ${instruction.toString()}"
}