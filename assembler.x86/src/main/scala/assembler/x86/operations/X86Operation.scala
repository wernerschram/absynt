package assembler.x86.operations

import assembler.Encodable
import assembler.Label
import assembler.LabeledEncodable
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.RexRequirement
import assembler.x86.operands.Operand
import assembler.x86.operands.SegmentRegister
import assembler.x86.operands.OperandSize
import assembler.x86.operands.Register
import assembler.x86.operands.ValueSize

trait X86Operation extends Encodable {
  def validate(): Unit = Unit

  def operands: List[Operand]

  override def size()(implicit page: MemoryPage): Int = encodeByte().length
  override def withLabel(label: Label): LabeledEncodable = new LabeledX86Operation(this, label)

  implicit val processorMode: ProcessorMode
  val includeRexW: Boolean = true
  def code: List[Byte]
  def mnemonic: String

  def operandSize: OperandSize = OperandSize.Unknown
  def addressSize: OperandSize = OperandSize.Unknown
  def segmentOverride: Option[SegmentRegister] = None
  def rexRequirements: List[RexRequirement] =
    if (includeRexW && operandSize == ValueSize.QuadWord) RexRequirement.quadOperand :: Nil else Nil

  private def optionalSegmentOverridePrefix: List[Byte] = segmentOverride match {
    case Some(segment) => X86Operation.SegmentOverrideMap.get(segment).toList
    case _ => Nil
  }

  private def optionalAddressSizePrefix()(implicit processorMode: ProcessorMode): List[Byte] =
    if (addressSize.requiresAddressSizePrefix(processorMode)) X86Operation.AddressSizeCode :: Nil else Nil

  private def optionalOperandSizePrefix()(implicit processorMode: ProcessorMode): List[Byte] =
    if (operandSize.requiresOperandSizePrefix(processorMode)) X86Operation.OperandSizeCode :: Nil else Nil

  private def optionalRexPrefix()(implicit processorMode: ProcessorMode): List[Byte] =
    if (rexRequirements.isEmpty)
      Nil
    else
      rexRequirements.foldLeft[Byte](X86Operation.RexCode)((value, req) => (value | req.rexBitmask).toByte) :: Nil

  override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
    validate()

    optionalSegmentOverridePrefix :::
      optionalAddressSizePrefix :::
      optionalOperandSizePrefix :::
      optionalRexPrefix :::
      code
  }

  override def toString = s"$mnemonic ${operands.reverseMap { operand => operand.toString }.mkString(", ")}"
}

object X86Operation {
  private val RexCode = 0x40.toByte

  private val OperandSizeCode = 0x66.toByte
  private val AddressSizeCode = 0x67.toByte

  private val SegmentOverrideMap: Map[SegmentRegister, Byte] = Map(
    (Register.CS, 0x2E.toByte),
    (Register.SS, 0x36.toByte),
    (Register.DS, 0x3E.toByte),
    (Register.ES, 0x26.toByte),
    (Register.FS, 0x64.toByte),
    (Register.GS, 0x65.toByte))
}
