package assembler.x86.instructions.jump

import assembler.Encodable
import assembler.LabelCondition
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.operands.FixedSizeOperand
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.ValueSize
import assembler.x86.operands.memoryaccess.FarPointer
import assembler.x86.operands.memoryaccess.MemoryLocation
import assembler.x86.operands.memoryaccess.NearPointer
import assembler.x86.operations.{ FarPointer => FarPointerOperation }
import assembler.x86.operations.ModRMStaticOperation
import assembler.x86.operations.{ NearPointer => NearPointerOperation }
import assembler.x86.operations.ShortJumpOperation
import assembler.x86.operations.ShortOrNearJumpOperation
import assembler.x86.operations.Static

abstract class ShortRelativeJump(val shortOpcode: List[Byte], implicit val mnemonic: String) {

  protected def Rel8(nearPointer: NearPointer)(implicit processorMode: ProcessorMode) = new Static(shortOpcode, mnemonic) with NearPointerOperation {
    override val pointer = nearPointer
  }

  def apply(nearPointer: NearPointer)(implicit processorMode: ProcessorMode) = {
    assume(nearPointer.operandByteSize == ValueSize.Byte)
    Rel8(nearPointer)
  }

  def apply(condition: LabelCondition)(implicit processorMode: ProcessorMode): Encodable =
    new ShortJumpOperation(shortOpcode, mnemonic, condition) {

      def encodeForPointer(nearPointer: NearPointer)(implicit page: MemoryPage): List[Byte] = {
        assume(nearPointer.operandByteSize == ValueSize.Byte)
        Rel8(nearPointer).encodeByte()
      }
    }
}

abstract class ShortOrLongRelativeJump(shortOpcode: List[Byte], val longOpcode: List[Byte], mnemonic: String) extends ShortRelativeJump(shortOpcode, mnemonic) {

  private def Rel16(nearPointer: NearPointer)(implicit processorMode: ProcessorMode) = new Static(longOpcode, mnemonic) with NearPointerOperation {
    override val pointer = nearPointer

    override def validate = {
      super.validate
      processorMode match {
        case ProcessorMode.Long | ProcessorMode.Protected => assume(pointer.operandByteSize != ValueSize.Word)
        case ProcessorMode.Real => assume(pointer.operandByteSize == ValueSize.Word)
      }
    }
  }

  override def apply(nearPointer: NearPointer)(implicit processorMode: ProcessorMode) = nearPointer.operandByteSize match {
    case ValueSize.Byte =>
      super.apply(nearPointer)
    case _ =>
      Rel16(nearPointer)
  }

  override def apply(condition: LabelCondition)(implicit processorMode: ProcessorMode) =
    new ShortOrNearJumpOperation(shortOpcode, longOpcode, mnemonic, condition) {
      override def encodeForPointer(nearPointer: NearPointer)(implicit page: MemoryPage): List[Byte] = Rel8(nearPointer).encodeByte()
      override def encodeForNearPointer(nearPointer: NearPointer)(implicit page: MemoryPage): List[Byte] = Rel16(nearPointer).encodeByte()
    }
}

final object Jump extends ShortOrLongRelativeJump(0xEB.toByte :: Nil, 0xE9.toByte :: Nil, "jmp") {

  private def RM16(operand: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRMStaticOperation(operand, 0xff.toByte :: Nil, 4, mnemonic, false) {
      assume((operandRM, processorMode) match {
        case (fixed: ModRMEncodableOperand with FixedSizeOperand, ProcessorMode.Long) if (fixed.operandByteSize != ValueSize.QuadWord) => false
        case (fixed: ModRMEncodableOperand with FixedSizeOperand, ProcessorMode.Real | ProcessorMode.Protected) if (fixed.operandByteSize == ValueSize.QuadWord) => false
        case _ => true
      })
    }

  private def Ptr1616(farPointer: FarPointer)(implicit processorMode: ProcessorMode) = new Static(0xEA.toByte :: Nil, mnemonic) with FarPointerOperation {
    override def pointer = farPointer
  }

  private def M1616(operand: MemoryLocation)(implicit processorMode: ProcessorMode) =
    new ModRMStaticOperation(operand, 0xFF.toByte :: Nil, 5, s"${mnemonic} FAR") {
      assume((operandRM, processorMode) match {
        case (fixed: ModRMEncodableOperand with FixedSizeOperand, ProcessorMode.Real | ProcessorMode.Protected) if (fixed.operandByteSize == ValueSize.QuadWord) => false
        case _ => true
      })
    }

  def apply(operand: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    RM16(operand)

  object Far {
    def apply(farPointer: FarPointer)(implicit processorMode: ProcessorMode) =
      Ptr1616(farPointer)

    def apply(pointer: MemoryLocation)(implicit processorMode: ProcessorMode) =
      M1616(pointer)
  }
}

private[jump] class JumpIfOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(0x70.toByte :: Nil, 0x0F.toByte :: 0x80.toByte :: Nil, mnemonic)
private[jump] class JumpIfNotOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(0x71.toByte :: Nil, 0x0F.toByte :: 0x81.toByte :: Nil, mnemonic)
private[jump] class JumpIfCarry(mnemonic: String)
  extends ShortOrLongRelativeJump(0x72.toByte :: Nil, 0x0F.toByte :: 0x82.toByte :: Nil, mnemonic)
private[jump] class JumpIfNoCarry(mnemonic: String)
  extends ShortOrLongRelativeJump(0x73.toByte :: Nil, 0x0F.toByte :: 0x83.toByte :: Nil, mnemonic)
private[jump] class JumpIfZero(mnemonic: String)
  extends ShortOrLongRelativeJump(0x74.toByte :: Nil, 0x0F.toByte :: 0x84.toByte :: Nil, mnemonic)
private[jump] class JumpIfNotZero(mnemonic: String)
  extends ShortOrLongRelativeJump(0x75.toByte :: Nil, 0x0F.toByte :: 0x85.toByte :: Nil, mnemonic)
private[jump] class JumpIfCarryOrZero(mnemonic: String)
  extends ShortOrLongRelativeJump(0x76.toByte :: Nil, 0x0F.toByte :: 0x86.toByte :: Nil, mnemonic)
private[jump] class JumpIfNoCarryAndNoZero(mnemonic: String)
  extends ShortOrLongRelativeJump(0x77.toByte :: Nil, 0x0F.toByte :: 0x87.toByte :: Nil, mnemonic)
private[jump] class JumpIfSigned(mnemonic: String)
  extends ShortOrLongRelativeJump(0x78.toByte :: Nil, 0x0F.toByte :: 0x88.toByte :: Nil, mnemonic)
private[jump] class JumpIfNotSigned(mnemonic: String)
  extends ShortOrLongRelativeJump(0x79.toByte :: Nil, 0x0F.toByte :: 0x8B.toByte :: Nil, mnemonic)
private[jump] class JumpIfParity(mnemonic: String)
  extends ShortOrLongRelativeJump(0x7A.toByte :: Nil, 0x0F.toByte :: 0x8A.toByte :: Nil, mnemonic)
private[jump] class JumpIfNotParity(mnemonic: String)
  extends ShortOrLongRelativeJump(0x7B.toByte :: Nil, 0x0F.toByte :: 0x8B.toByte :: Nil, mnemonic)
private[jump] class JumpIfSignedNotEqualsOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(0x7C.toByte :: Nil, 0x0F.toByte :: 0x8C.toByte :: Nil, mnemonic)
private[jump] class JumpIfSignedEqualsOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(0x7D.toByte :: Nil, 0x0F.toByte :: 0x8D.toByte :: Nil, mnemonic)
private[jump] class JumpIfZeroAndSignedNotEqualsOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(0x7E.toByte :: Nil, 0x0F.toByte :: 0x8E.toByte :: Nil, mnemonic)
private[jump] class JumpIfNotZeroAndSignedEqualsOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(0x7F.toByte :: Nil, 0x0F.toByte :: 0x8F.toByte :: Nil, mnemonic)
private[jump] class JumpIfCountZero
  extends ShortRelativeJump(0xE3.toByte :: Nil, "jcx")

final object JumpIfAbove extends JumpIfNoCarryAndNoZero("ja")
final object JumpIfAboveOrEqual extends JumpIfNoCarry("jae")
final object JumpIfBelow extends JumpIfCarry("jb")
final object JumpIfBelowOrEqual extends JumpIfCarryOrZero("jbe")
final object JumpIfCarry extends JumpIfCarry("jc")
final object JumpIfCountZero extends JumpIfCountZero
final object JumpIfEqual extends JumpIfZero("je")
final object JumpIfGreater extends JumpIfNotZeroAndSignedEqualsOverflow("jg")
final object JumpIfGreaterOrEqual extends JumpIfSignedEqualsOverflow("jge")
final object JumpIfLess extends JumpIfSignedNotEqualsOverflow("jl")
final object JumpIfLessOrEqual extends JumpIfZeroAndSignedNotEqualsOverflow("jle")
final object JumpIfNotAbove extends JumpIfCarryOrZero("jna")
final object JumpIfNotAboveOrEqual extends JumpIfCarry("jnae")
final object JumpIfNotBelow extends JumpIfNoCarry("jnb")
final object JumpIfNotBelowOrEqual extends JumpIfNoCarryAndNoZero("jnbe")
final object JumpIfNoCarry extends JumpIfNoCarry("jnc")
final object JumpIfNotEqual extends JumpIfNotZero("jne")
final object JumpIfNotGreater extends JumpIfZeroAndSignedNotEqualsOverflow("jng")
final object JumpIfNotGreaterOrEqual extends JumpIfSignedNotEqualsOverflow("jnge")
final object JumpIfNotLess extends JumpIfSignedEqualsOverflow("jnl")
final object JumpIfNotLessOrEqual extends JumpIfNotZeroAndSignedEqualsOverflow("jnle")
final object JumpIfNotOverflow extends JumpIfNotOverflow("jno")
final object JumpIfNotParity extends JumpIfNotParity("jnp")
final object JumpIfNotSigned extends JumpIfNotSigned("jns")
final object JumpIfNotZero extends JumpIfNotZero("jnz")
final object JumpIfOverflow extends JumpIfOverflow("jo")
final object JumpIfParity extends JumpIfParity("jp")
final object JumpIfParityEven extends JumpIfParity("jpe")
final object JumpIfParityOdd extends JumpIfNotParity("jpo")
final object JumpIfSigned extends JumpIfSigned("js")
final object JumpIfZero extends JumpIfZero("jz")