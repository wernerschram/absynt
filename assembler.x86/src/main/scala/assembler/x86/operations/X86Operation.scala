package assembler.x86.operations

import assembler.x86.operands._
import assembler.x86.{ProcessorMode, RexRequirement}
import assembler.resource.UnlabeledEncodable

abstract class OperandInfo(operand: Operand) {
  override def toString: String = operand.toString
}

object OperandInfo {
  def pointer(pointer: memoryaccess.FarPointer[_]): OperandInfo = new OperandInfo(pointer) {} //ptrXX

  def relative(pointer: memoryaccess.NearPointer[_]): OperandInfo = new OperandInfo(pointer) {} //relXX

  def immediate(immediate: ImmediateValue): OperandInfo = new OperandInfo(immediate) {} //immXX

  def implicitOperand(operand: Operand): OperandInfo = new OperandInfo(operand) {} //XX

  def encodedRegister(register: GeneralPurposeRegister): OperandInfo = new OperandInfo(register) {} //rX

  def memoryOffset(offset: memoryaccess.MemoryLocation): OperandInfo = new OperandInfo(offset) {} //moffsXX

  def rmRegisterOrMemory(rm: ModRMEncodableOperand): OperandInfo = new OperandInfo(rm) {} //r/mXX

  def rmRegister(register: GeneralPurposeRegister): OperandInfo = new OperandInfo(register) {} //rXX

  def rmSegment(register: SegmentRegister): OperandInfo = new OperandInfo(register) {} //SregXX

}

abstract class X86Operation extends UnlabeledEncodable {
  val includeRexW: Boolean = true

  def operands: Seq[OperandInfo]

  override def size: Int = encodeByte.length

  override def encodeByte: Seq[Byte] = {
    validate()

    optionalSegmentOverridePrefix ++
      optionalAddressSizePrefix ++
      optionalOperandSizePrefix ++
      optionalRexPrefix ++
      code
  }

  implicit val processorMode: ProcessorMode

  def validate(): Unit = Unit

  private def optionalSegmentOverridePrefix: List[Byte] = segmentOverride match {
    case Some(segment) => X86Operation.SegmentOverrideMap.get(segment).toList
    case _ => Nil
  }

  def segmentOverride: Option[SegmentRegister] = None

  private def optionalAddressSizePrefix: List[Byte] =
    if (addressSize.requiresAddressSizePrefix(processorMode)) X86Operation.AddressSizeCode :: Nil else Nil

  def addressSize: OperandSize = OperandSize.Unknown

  private def optionalOperandSizePrefix: List[Byte] =
    if (operandSize.requiresOperandSizePrefix(processorMode)) X86Operation.OperandSizeCode :: Nil else Nil

  def operandSize: OperandSize = OperandSize.Unknown

  private def optionalRexPrefix: List[Byte] = {
    assume(processorMode == ProcessorMode.Long || rexRequirements.isEmpty)
    if (rexRequirements.isEmpty)
      Nil
    else
      rexRequirements.foldLeft[Byte](X86Operation.RexCode)((value, req) => (value | req.rexBitMask).toByte) :: Nil
  }

  def rexRequirements: Seq[RexRequirement] =
    if (includeRexW && operandSize == ValueSize.QuadWord) RexRequirement.quadOperand :: Nil else Nil

  def code: Seq[Byte]

  def mnemonic: String

  override def toString: String = if (operands.isEmpty)
      s"$mnemonic"
    else
      s"$mnemonic ${operands.reverseMap { operand => operand.toString }.mkString(", ")}"
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
