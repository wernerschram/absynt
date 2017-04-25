package assembler.x86.instructions

import assembler.sections.Section
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.{FarPointer, MemoryLocation, NearPointer}
import assembler.x86.operands.{FixedSizeOperand, ModRMEncodableOperand, ValueSize}
import assembler.x86.operations.{ModRMStatic, NearJumpOperation, ShortJumpOperation, Static,
                                 FarPointer => FarPointerOperation, NearPointer => NearPointerOperation}
import assembler.{Encodable, Label}

abstract class ShortRelativeJump(val shortOpcode: List[Byte], implicit val mnemonic: String) {

  def apply(nearPointer: NearPointer)(implicit label: Label, processorMode: ProcessorMode): Static with NearPointerOperation = {
    assume(nearPointer.operandByteSize == ValueSize.Byte)
    Rel8(nearPointer)
  }

  def apply(targetLabel: Label)(implicit label: Label, processorMode: ProcessorMode): Encodable =
    new ShortJumpOperation(label, shortOpcode, mnemonic, targetLabel) {

      def encodableForShortPointer(nearPointer: NearPointer)(implicit page: Section): Encodable = {
        assume(nearPointer.operandByteSize == ValueSize.Byte)
        Rel8(nearPointer)
      }
    }

  protected def Rel8(nearPointer: NearPointer)(implicit label: Label, processorMode: ProcessorMode) =
    new Static(label, shortOpcode, mnemonic) with NearPointerOperation {
      override val pointer: NearPointer = nearPointer
    }
}

abstract class ShortOrLongRelativeJump(shortOpcode: List[Byte], val longOpcode: List[Byte], mnemonic: String)
  extends ShortRelativeJump(shortOpcode, mnemonic) {

  override def apply(nearPointer: NearPointer)(implicit label: Label, processorMode: ProcessorMode): Static with NearPointerOperation =
    nearPointer.operandByteSize match {
      case ValueSize.Byte =>
        super.apply(nearPointer)
      case _ =>
        Rel16(nearPointer)
    }

  private def Rel16(nearPointer: NearPointer)(implicit label: Label, processorMode: ProcessorMode) =
    new Static(label, longOpcode, mnemonic) with NearPointerOperation {
      override val pointer: NearPointer = nearPointer

      override def validate(): Unit = {
        super.validate()
        processorMode match {
          case ProcessorMode.Long | ProcessorMode.Protected => assume(pointer.operandByteSize != ValueSize.Word)
          case ProcessorMode.Real => assume(pointer.operandByteSize == ValueSize.Word)
        }
      }
    }

  override def apply(targetLabel: Label)(implicit label: Label, processorMode: ProcessorMode) =
    new NearJumpOperation(label, shortOpcode, longOpcode, mnemonic, targetLabel) {
      override def encodableForShortPointer(nearPointer: NearPointer)(implicit page: Section): Encodable = Rel8(nearPointer)

      override def encodableForLongPointer(nearPointer: NearPointer)(implicit page: Section): Encodable = Rel16(nearPointer)
    }
}

object Jump extends ShortOrLongRelativeJump(0xEB.toByte :: Nil, 0xE9.toByte :: Nil, "jmp") {

  def apply(operand: ModRMEncodableOperand)(implicit label: Label, processorMode: ProcessorMode) =
    RM16(operand)

  private def RM16(operand: ModRMEncodableOperand)(implicit label: Label, processorMode: ProcessorMode) =
    new ModRMStatic(label, operand, 0xff.toByte :: Nil, 4, mnemonic, false) {
      assume((operandRM, processorMode) match {
        case (fixed: ModRMEncodableOperand with FixedSizeOperand, ProcessorMode.Long)
          if fixed.operandByteSize != ValueSize.QuadWord => false
        case (fixed: ModRMEncodableOperand with FixedSizeOperand, ProcessorMode.Real | ProcessorMode.Protected)
          if fixed.operandByteSize == ValueSize.QuadWord => false
        case _ => true
      })
    }

  private def Ptr1616(farPointer: FarPointer)(implicit label: Label, processorMode: ProcessorMode) =
    new Static(label, 0xEA.toByte :: Nil, mnemonic) with FarPointerOperation {
      override def pointer: FarPointer = farPointer
    }

  private def M1616(operand: MemoryLocation)(implicit label: Label, processorMode: ProcessorMode) =
    new ModRMStatic(label, operand, 0xFF.toByte :: Nil, 5, s"$mnemonic FAR") {
      assume((operandRM, processorMode) match {
        case (fixed: ModRMEncodableOperand with FixedSizeOperand, ProcessorMode.Real | ProcessorMode.Protected)
          if fixed.operandByteSize == ValueSize.QuadWord => false
        case _ => true
      })
    }

  object Far {
    def apply(farPointer: FarPointer)(implicit label: Label, processorMode: ProcessorMode) =
      Ptr1616(farPointer)

    def apply(pointer: MemoryLocation)(implicit label: Label, processorMode: ProcessorMode) =
      M1616(pointer)
  }

}

private[instructions] class JumpIfOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(0x70.toByte :: Nil, 0x0F.toByte :: 0x80.toByte :: Nil, mnemonic)

private[instructions] class JumpIfNotOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(0x71.toByte :: Nil, 0x0F.toByte :: 0x81.toByte :: Nil, mnemonic)

private[instructions] class JumpIfCarry(mnemonic: String)
  extends ShortOrLongRelativeJump(0x72.toByte :: Nil, 0x0F.toByte :: 0x82.toByte :: Nil, mnemonic)

private[instructions] class JumpIfNoCarry(mnemonic: String)
  extends ShortOrLongRelativeJump(0x73.toByte :: Nil, 0x0F.toByte :: 0x83.toByte :: Nil, mnemonic)

private[instructions] class JumpIfZero(mnemonic: String)
  extends ShortOrLongRelativeJump(0x74.toByte :: Nil, 0x0F.toByte :: 0x84.toByte :: Nil, mnemonic)

private[instructions] class JumpIfNotZero(mnemonic: String)
  extends ShortOrLongRelativeJump(0x75.toByte :: Nil, 0x0F.toByte :: 0x85.toByte :: Nil, mnemonic)

private[instructions] class JumpIfCarryOrZero(mnemonic: String)
  extends ShortOrLongRelativeJump(0x76.toByte :: Nil, 0x0F.toByte :: 0x86.toByte :: Nil, mnemonic)

private[instructions] class JumpIfNoCarryAndNoZero(mnemonic: String)
  extends ShortOrLongRelativeJump(0x77.toByte :: Nil, 0x0F.toByte :: 0x87.toByte :: Nil, mnemonic)

private[instructions] class JumpIfSigned(mnemonic: String)
  extends ShortOrLongRelativeJump(0x78.toByte :: Nil, 0x0F.toByte :: 0x88.toByte :: Nil, mnemonic)

private[instructions] class JumpIfNotSigned(mnemonic: String)
  extends ShortOrLongRelativeJump(0x79.toByte :: Nil, 0x0F.toByte :: 0x8B.toByte :: Nil, mnemonic)

private[instructions] class JumpIfParity(mnemonic: String)
  extends ShortOrLongRelativeJump(0x7A.toByte :: Nil, 0x0F.toByte :: 0x8A.toByte :: Nil, mnemonic)

private[instructions] class JumpIfNotParity(mnemonic: String)
  extends ShortOrLongRelativeJump(0x7B.toByte :: Nil, 0x0F.toByte :: 0x8B.toByte :: Nil, mnemonic)

private[instructions] class JumpIfSignedNotEqualsOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(0x7C.toByte :: Nil, 0x0F.toByte :: 0x8C.toByte :: Nil, mnemonic)

private[instructions] class JumpIfSignedEqualsOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(0x7D.toByte :: Nil, 0x0F.toByte :: 0x8D.toByte :: Nil, mnemonic)

private[instructions] class JumpIfZeroAndSignedNotEqualsOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(0x7E.toByte :: Nil, 0x0F.toByte :: 0x8E.toByte :: Nil, mnemonic)

private[instructions] class JumpIfNotZeroAndSignedEqualsOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(0x7F.toByte :: Nil, 0x0F.toByte :: 0x8F.toByte :: Nil, mnemonic)

private[instructions] class JumpIfCountZero
  extends ShortRelativeJump(0xE3.toByte :: Nil, "jcx")

object JumpIfAbove extends JumpIfNoCarryAndNoZero("ja")
object JumpIfAboveOrEqual extends JumpIfNoCarry("jae")
object JumpIfBelow extends JumpIfCarry("jb")
object JumpIfBelowOrEqual extends JumpIfCarryOrZero("jbe")
object JumpIfCarry extends JumpIfCarry("jc")
object JumpIfCountZero extends JumpIfCountZero
object JumpIfEqual extends JumpIfZero("je")
object JumpIfGreater extends JumpIfNotZeroAndSignedEqualsOverflow("jg")
object JumpIfGreaterOrEqual extends JumpIfSignedEqualsOverflow("jge")
object JumpIfLess extends JumpIfSignedNotEqualsOverflow("jl")
object JumpIfLessOrEqual extends JumpIfZeroAndSignedNotEqualsOverflow("jle")
object JumpIfNotAbove extends JumpIfCarryOrZero("jna")
object JumpIfNotAboveOrEqual extends JumpIfCarry("jnae")
object JumpIfNotBelow extends JumpIfNoCarry("jnb")
object JumpIfNotBelowOrEqual extends JumpIfNoCarryAndNoZero("jnbe")
object JumpIfNoCarry extends JumpIfNoCarry("jnc")
object JumpIfNotEqual extends JumpIfNotZero("jne")
object JumpIfNotGreater extends JumpIfZeroAndSignedNotEqualsOverflow("jng")
object JumpIfNotGreaterOrEqual extends JumpIfSignedNotEqualsOverflow("jnge")
object JumpIfNotLess extends JumpIfSignedEqualsOverflow("jnl")
object JumpIfNotLessOrEqual extends JumpIfNotZeroAndSignedEqualsOverflow("jnle")
object JumpIfNotOverflow extends JumpIfNotOverflow("jno")
object JumpIfNotParity extends JumpIfNotParity("jnp")
object JumpIfNotSigned extends JumpIfNotSigned("jns")
object JumpIfNotZero extends JumpIfNotZero("jnz")
object JumpIfOverflow extends JumpIfOverflow("jo")
object JumpIfParity extends JumpIfParity("jp")
object JumpIfParityEven extends JumpIfParity("jpe")
object JumpIfParityOdd extends JumpIfNotParity("jpo")
object JumpIfSigned extends JumpIfSigned("js")
object JumpIfZero extends JumpIfZero("jz")