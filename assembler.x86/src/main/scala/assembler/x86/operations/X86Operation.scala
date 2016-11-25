package assembler.x86.operations

import assembler.Encodable
import assembler.Label
import assembler.LabeledEncodable
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.RexExtendedRequirement
import assembler.x86.operands.Operand
import assembler.x86.operands.SegmentRegister

trait X86Operation extends Encodable {
  def validate: Unit = Unit

  def operands: List[Operand]

  override def size()(implicit page: MemoryPage) = encodeByte().length
  override def withLabel(label: Label): LabeledEncodable = new LabeledX86Operation(this, label)

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
