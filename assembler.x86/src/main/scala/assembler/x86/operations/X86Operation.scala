package assembler.x86.operations

import assembler.Label
import assembler.x86.operands._
import assembler.x86.{ProcessorMode, RexRequirement}
import assembler.resource.Encodable

abstract class X86Operation(label: Label) extends Encodable(label) {
  val includeRexW: Boolean = true

  def operands: List[Operand]

  override def size: Int = encodeByte.length

  override def encodeByte: List[Byte] = {
    validate()

    optionalSegmentOverridePrefix :::
      optionalAddressSizePrefix :::
      optionalOperandSizePrefix :::
      optionalRexPrefix :::
      code
  }

  implicit val processorMode: ProcessorMode

  def validate(): Unit = Unit

  private def optionalSegmentOverridePrefix: List[Byte] = segmentOverride match {
    case Some(segment) => X86Operation.SegmentOverrideMap.get(segment).toList
    case _ => Nil
  }

  def segmentOverride: Option[SegmentRegister] = None

  private def optionalAddressSizePrefix()(implicit processorMode: ProcessorMode): List[Byte] =
    if (addressSize.requiresAddressSizePrefix(processorMode)) X86Operation.AddressSizeCode :: Nil else Nil

  def addressSize: OperandSize = OperandSize.Unknown

  private def optionalOperandSizePrefix()(implicit processorMode: ProcessorMode): List[Byte] =
    if (operandSize.requiresOperandSizePrefix(processorMode)) X86Operation.OperandSizeCode :: Nil else Nil

  def operandSize: OperandSize = OperandSize.Unknown

  private def optionalRexPrefix()(implicit processorMode: ProcessorMode): List[Byte] = {
    assume(processorMode == ProcessorMode.Long || rexRequirements.isEmpty)
    if (rexRequirements.isEmpty)
      Nil
    else
      rexRequirements.foldLeft[Byte](X86Operation.RexCode)((value, req) => (value | req.rexBitMask).toByte) :: Nil
  }

  def rexRequirements: List[RexRequirement] =
    if (includeRexW && operandSize == ValueSize.QuadWord) RexRequirement.quadOperand :: Nil else Nil

  def code: List[Byte]

  def mnemonic: String

  override def toString = if (operands.isEmpty)
      s"$labelPrefix$mnemonic"
    else
      s"$labelPrefix$mnemonic ${operands.reverseMap { operand => operand.toString }.mkString(", ")}"
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
