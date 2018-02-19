package assembler.x86.operations

import assembler.resource.UnlabeledEncodable
import assembler.x86.operands._
import assembler.x86.operands.memoryaccess.RegisterMemoryLocation
import assembler.x86.{ProcessorMode, RexRequirement}

sealed abstract class OperandInfo(val operand: Operand, val order: OperandInfo.OperandOrder.Value) extends Ordered[OperandInfo] {
  override def toString: String = operand.toString

  override def compare(that: OperandInfo): Int = order compare that.order

  def requiresOperandSize(processorMode: ProcessorMode): Boolean = false

  def requiresAddressSize(processorMode: ProcessorMode): Boolean = false

  def rexRequirements: Seq[RexRequirement] = operand match {
    case f: FixedSizeOperand if f.operandByteSize == ValueSize.QuadWord => Seq(RexRequirement.quadOperand)
    case _ => Seq.empty
  }
}

object OperandInfo {
  object OperandOrder extends Enumeration {
    type OperandOrder = Value
    val first, second, third = Value
  }

  import OperandOrder._

  def pointer(pointer: memoryaccess.FarPointer[_], operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(pointer, operandOrder) {
      override def requiresOperandSize(processorMode: ProcessorMode): Boolean =
        pointer.operandByteSize == FarPointerSize.DoubleWord && processorMode != ProcessorMode.Real ||
        pointer.operandByteSize == FarPointerSize.FarWord && processorMode == ProcessorMode.Real
    } //ptrXX

  def relative(pointer: memoryaccess.NearPointer, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(pointer, operandOrder) {
   } //relXX

  def immediate(immediate: ImmediateValue, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(immediate, operandOrder) {
      override def requiresOperandSize(processorMode: ProcessorMode): Boolean =
        immediate.operandByteSize == ValueSize.Word && processorMode != ProcessorMode.Real ||
        immediate.operandByteSize == ValueSize.DoubleWord && processorMode == ProcessorMode.Real
    } //immXX

  def implicitOperand(operand: Operand, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(operand, operandOrder) {
     override def requiresOperandSize(processorMode: ProcessorMode): Boolean = operand match {
        case f: FixedSizeOperand =>
          f.operandByteSize == ValueSize.Word && processorMode != ProcessorMode.Real ||
          f.operandByteSize == ValueSize.DoubleWord && processorMode == ProcessorMode.Real
        case _ => false
      }
    } //XX

  def implicitPort(operand: Operand, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(operand, operandOrder) { } //XX

  def implicitAddress(operand: Operand, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(operand, operandOrder) {
     override def requiresAddressSize(processorMode: ProcessorMode): Boolean = operand match {
        case f: memoryaccess.MemoryLocation =>
          f.addressSize == ValueSize.Word && processorMode == ProcessorMode.Protected ||
          f.addressSize == ValueSize.DoubleWord && processorMode != ProcessorMode.Protected
        case _ => false
      }
    } //XX

  def encodedRegister(register: GeneralPurposeRegister, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(register, operandOrder) {
      override def requiresOperandSize(processorMode: ProcessorMode): Boolean =
        register.operandByteSize == ValueSize.Word && processorMode != ProcessorMode.Real ||
        register.operandByteSize == ValueSize.DoubleWord && processorMode == ProcessorMode.Real

      override def rexRequirements: Seq[RexRequirement] = register match {
        case r: GeneralPurposeRexRegister => Seq(RexRequirement.instanceOpcodeReg) // +: super.rexRequirements
        case _ => Seq.empty//super.rexRequirements
      }

    } //rX

  def memoryOffset(offset: memoryaccess.MemoryLocation, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(offset, operandOrder) {
      override def requiresAddressSize(processorMode: ProcessorMode): Boolean =
          offset.addressSize == ValueSize.Word && processorMode == ProcessorMode.Protected ||
          offset.addressSize == ValueSize.DoubleWord && processorMode != ProcessorMode.Protected
    } //moffsXX

  def rmRegisterOrMemory(rm: ModRMEncodableOperand, operandOrder: OperandOrder, includeRexW: Boolean): OperandInfo =
    new OperandInfo(rm, operandOrder) {
      override def requiresOperandSize(processorMode: ProcessorMode): Boolean = rm match {
        case f: FixedSizeOperand =>
          f.operandByteSize == ValueSize.Word && processorMode != ProcessorMode.Real ||
            f.operandByteSize == ValueSize.DoubleWord && processorMode == ProcessorMode.Real
        case _ => false
      }

      override def requiresAddressSize(processorMode: ProcessorMode): Boolean = rm match {
        case f: memoryaccess.MemoryLocation =>
          f.addressSize == ValueSize.Word && processorMode == ProcessorMode.Protected ||
            f.addressSize == ValueSize.DoubleWord && processorMode != ProcessorMode.Protected
        case _ => false
      }

      override def rexRequirements: Seq[RexRequirement] = {
        val operandRequirements = rm match {
          case _: GeneralPurposeRexRegister =>
            Seq(RexRequirement.instanceOperandRM)
          case r: RegisterMemoryLocation[_] if r.reference.isInstanceOf[GeneralPurposeRexRegister with ProtectedModeIndexRegister] =>
            Seq(RexRequirement.instanceOperandRM)
          case r: memoryaccess.SIBMemoryLocation =>
            val addressRequirements: Seq[RexRequirement] = Seq(
              r.index match {
                case _: GeneralPurposeRexRegister => Seq(RexRequirement.instanceIndex)
                case _ => Seq.empty[RexRequirement]
              },
              r.base match {
                case _: GeneralPurposeRexRegister => Seq(RexRequirement.instanceBase)
                case _ => Seq.empty[RexRequirement]
              }).flatten

            addressRequirements
          case _ => Seq.empty
        }
        if (includeRexW)
          operandRequirements ++ super.rexRequirements
        else
          operandRequirements
      }
    }//r/mXX

  def rmRegister(register: GeneralPurposeRegister, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(register, operandOrder) {
      override def requiresOperandSize(processorMode: ProcessorMode): Boolean =
        register.operandByteSize == ValueSize.Word && processorMode != ProcessorMode.Real ||
        register.operandByteSize == ValueSize.DoubleWord && processorMode == ProcessorMode.Real

      override def rexRequirements: Seq[RexRequirement] = register match {
        case r: GeneralPurposeRexRegister => RexRequirement.instanceOperandR +: super.rexRequirements
        case _ => super.rexRequirements
      }
    } //rXX

  def rmSegment(register: SegmentRegister, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(register, operandOrder) {} //SregXX

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
    if (operands.exists(o => o.requiresAddressSize(processorMode))) X86Operation.AddressSizeCode :: Nil else Nil

  private def optionalOperandSizePrefix: List[Byte] =
    if (operands.exists(o => o.requiresOperandSize(processorMode))) X86Operation.OperandSizeCode :: Nil else Nil

  private def optionalRexPrefix: List[Byte] = {
    assume(processorMode == ProcessorMode.Long || rexRequirements.isEmpty)
    if (rexRequirements.isEmpty)
      Nil
    else
      rexRequirements.foldLeft[Byte](X86Operation.RexCode)((value, req) => (value | req.rexBitMask).toByte) :: Nil
  }

  def rexRequirements: Seq[RexRequirement] =
    operands.flatMap(_.rexRequirements)

  def code: Seq[Byte]

  def mnemonic: String

  override def toString: String = if (operands.isEmpty)
      s"$mnemonic"
    else
      s"$mnemonic ${operands.sorted.map{ _.toString }.mkString(", ")}"
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
