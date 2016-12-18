package assembler.x86.operations

import assembler.Encodable
import assembler.Label
import assembler.LabeledEncodable
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.RexExtendedRequirement
import assembler.x86.operands.Operand
import assembler.x86.operands.SegmentRegister
import assembler.x86.operands.OperandSize
import assembler.x86.operands.Register
import assembler.x86.operands.ValueSize

trait X86Operation extends Encodable {
  def validate: Unit = Unit

  def operands: List[Operand]

  override def size()(implicit page: MemoryPage) = encodeByte().length
  override def withLabel(label: Label): LabeledEncodable = new LabeledX86Operation(this, label)

  implicit val processorMode: ProcessorMode
  val includeRexW: Boolean = true
  def code: List[Byte]
  def mnemonic: String

  def operandSize: OperandSize = OperandSize.Unknown
  def addressSize: OperandSize = OperandSize.Unknown
  def segmentOverride: Option[SegmentRegister] = None
  def rexRequirements: List[RexExtendedRequirement] = Nil

  private def optionalSegmentOverridePrefix: List[Byte] = segmentOverride match {
    case Some(segment) => X86Operation.SegmentOverrideMap.get(segment).toList
    case _ => Nil
  }

  private def optionalAddressSizePrefix()(implicit processorMode: ProcessorMode): List[Byte] =
    if (addressSize.requiresAddressSizePrefix(processorMode)) X86Operation.AddressSizeCode :: Nil else Nil

  private def optionalOperandSizePrefix()(implicit processorMode: ProcessorMode): List[Byte] =
    if (operandSize.requiresOperandSizePrefix(processorMode)) X86Operation.OperandSizeCode :: Nil else Nil

  private def optionalRexPrefix()(implicit processorMode: ProcessorMode): List[Byte] = {
    val rexW = (includeRexW && operandSize == ValueSize.QuadWord)
    if (rexRequirements.isEmpty && (!rexW)) {
      Nil
    } else {
      val rexPrefix = rexRequirements.foldLeft[Byte](X86Operation.RexCode)((value, req) => (value | req.rexBitmask).toByte)

      if (rexW) {
        (rexPrefix | X86Operation.RexWBitValue).toByte :: Nil
      } else {
        rexPrefix :: Nil
      }
    }
  }


  override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
    validate

    optionalSegmentOverridePrefix :::
      optionalAddressSizePrefix :::
      optionalOperandSizePrefix :::
      optionalRexPrefix :::
      code
  }

  override def toString() = s"${mnemonic} ${operands.reverseMap { operand => operand.toString() }.mkString(", ")}"
}


object X86Operation {
  private val RexCode = 0x40.toByte
  private val RexWBitValue: Byte = 8

  private val OperandSizeCode = 0x66.toByte
  private val AddressSizeCode = 0x67.toByte

    private val SegmentOverrideCS = 0x2E.toByte
  private val SegmentOverrideSS = 0x36.toByte
  private val SegmentOverrideDS = 0x3E.toByte
  private val SegmentOverrideES = 0x26.toByte
  private val SegmentOverrideFS = 0x64.toByte
  private val SegmentOverrideGS = 0x65.toByte

  private val SegmentOverrideMap: Map[SegmentRegister, Byte] = Map(
    (Register.CS, X86Operation.SegmentOverrideCS),
    (Register.SS, X86Operation.SegmentOverrideSS),
    (Register.DS, X86Operation.SegmentOverrideDS),
    (Register.ES, X86Operation.SegmentOverrideES),
    (Register.FS, X86Operation.SegmentOverrideFS),
    (Register.GS, X86Operation.SegmentOverrideGS))

}