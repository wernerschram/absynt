package assembler.x86.instructions

import assembler.Label
import assembler.resource.{Resource, UnlabeledEncodable}
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess._
import assembler.x86.operands._
import assembler.x86.operations.OperandInfo.OperandOrder._
import assembler.x86.operations.{ModRMStatic, NearJumpOperation, ShortJumpOperation, Static, X86Operation, FarPointer => FarPointerOperation, NearPointer => NearPointerOperation}

abstract class ShortRelativeJump(val shortOpcode: Seq[Byte], implicit val mnemonic: String) {

  def short(nearPointer: NearPointer with ByteSize)(implicit processorMode: ProcessorMode): X86Operation =
    Rel8(nearPointer)

  def apply(targetLabel: Label)(implicit processorMode: ProcessorMode): ShortJumpOperation = {
    new ShortJumpOperation(shortOpcode, mnemonic, targetLabel) {
      override def encodableForShortPointer(nearPointer: NearPointer with ByteSize): Resource with UnlabeledEncodable =
        Rel8(nearPointer)
    }
  }

  protected def Rel8(nearPointer: NearPointer with ByteSize)(implicit processorMode: ProcessorMode): X86Operation =
    new Static(shortOpcode, mnemonic) with NearPointerOperation {
      override val pointer: NearPointer = nearPointer

      override def pointerOrder: OperandOrder = destination
    }
}

trait ShortJumpApply {
  self: ShortRelativeJump =>

  def apply(nearPointer: NearPointer with ByteSize)(implicit processorMode: ProcessorMode): X86Operation =
    self.short(nearPointer)

}

abstract class ShortOrLongRelativeJump(shortOpcode: Seq[Byte], val longOpcode: Seq[Byte], mnemonic: String)
  extends ShortRelativeJump(shortOpcode, mnemonic) {

  def apply(nearPointer: NearPointer with ValueSize)(implicit processorMode: ProcessorMode): X86Operation =
    (processorMode, nearPointer) match {
      case (_, p: NearPointer with ByteSize) =>
        short(p)
      case (ProcessorMode.Real, p: NearPointer with WordSize) =>
        long(p)
      case (ProcessorMode.Protected | ProcessorMode.Long, p: NearPointer with DoubleWordSize) =>
        long(p)
      case _ => throw new AssertionError
    }

  def long(nearPointer: NearPointer with WideSize)(implicit processorMode: ProcessorMode): X86Operation =
    Rel16(nearPointer)

  private def Rel16(nearPointer: NearPointer)(implicit processorMode: ProcessorMode) = {
    new Static(longOpcode, mnemonic) with NearPointerOperation {
      override val pointer: NearPointer = nearPointer
      override def pointerOrder: OperandOrder = destination
    }
  }

  override def apply(targetLabel: Label)(implicit processorMode: ProcessorMode): NearJumpOperation = {
    new NearJumpOperation(shortOpcode, longOpcode, mnemonic, targetLabel) {
      override def encodableForShortPointer(nearPointer: NearPointer with ByteSize): Resource with UnlabeledEncodable =
        Rel8(nearPointer)

      override def encodableForLongPointer(nearPointer: NearPointer): Resource with UnlabeledEncodable = Rel16(nearPointer)
    }
  }
}

object Jump extends ShortOrLongRelativeJump(0xEB.toByte :: Nil, 0xE9.toByte :: Nil, "jmp") {

  def apply(operand: ModRMEncodableOperand)(implicit processorMode: ProcessorMode): ModRMStatic =
    (operand, processorMode) match {
      case (o: ModRMEncodableOperand with ExtendedSize, ProcessorMode.Real | ProcessorMode.Protected) =>
        RM16(o)
      case (o: ModRMEncodableOperand with QuadWordSize, ProcessorMode.Long) =>
        RM16(o)
      case (o: ModRMEncodableOperand with ValueSize, _) =>
        throw new AssertionError
      case _ =>
        RM16(operand)
    }

  private def RM16(operand: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRMStatic(operand, 0xff.toByte :: Nil, 4, mnemonic, false) {
      override def operandRMOrder: OperandOrder = destination
    }

  private def Ptr1616(farPointer: FarPointer)(implicit processorMode: ProcessorMode) =
    new Static(0xEA.toByte :: Nil, mnemonic) with FarPointerOperation {
      override def pointer: FarPointer = farPointer
    }

  private def M1616(operand: MemoryLocation with WideSize)(implicit processorMode: ProcessorMode) =
    new ModRMStatic(operand, 0xFF.toByte :: Nil, 5, s"$mnemonic FAR") {
      override def operandRMOrder: OperandOrder = destination
    }

  object Far {
    def apply(farPointer: FarPointer)(implicit processorMode: ProcessorMode): Static with FarPointerOperation =
      Ptr1616(farPointer)

    def apply(pointer: MemoryLocation with WideSize)(implicit processorMode: ProcessorMode): ModRMStatic =
      (pointer, processorMode) match {
        case (_: QuadWordSize, ProcessorMode.Protected | ProcessorMode.Real) =>
          throw new AssertionError
        case _ =>
          M1616(pointer)
      }
  }

}

private[instructions] class JumpIfOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(Seq(0x70.toByte), Seq(0x0F.toByte, 0x80.toByte), mnemonic)

private[instructions] class JumpIfNotOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(Seq(0x71.toByte), Seq(0x0F.toByte, 0x81.toByte), mnemonic)

private[instructions] class JumpIfCarry(mnemonic: String)
  extends ShortOrLongRelativeJump(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), mnemonic)

private[instructions] class JumpIfNoCarry(mnemonic: String)
  extends ShortOrLongRelativeJump(Seq(0x73.toByte), Seq(0x0F.toByte, 0x83.toByte), mnemonic)

private[instructions] class JumpIfZero(mnemonic: String)
  extends ShortOrLongRelativeJump(Seq(0x74.toByte), Seq(0x0F.toByte, 0x84.toByte), mnemonic)

private[instructions] class JumpIfNotZero(mnemonic: String)
  extends ShortOrLongRelativeJump(Seq(0x75.toByte), Seq(0x0F.toByte, 0x85.toByte), mnemonic)

private[instructions] class JumpIfCarryOrZero(mnemonic: String)
  extends ShortOrLongRelativeJump(Seq(0x76.toByte), Seq(0x0F.toByte, 0x86.toByte), mnemonic)

private[instructions] class JumpIfNoCarryAndNoZero(mnemonic: String)
  extends ShortOrLongRelativeJump(Seq(0x77.toByte), Seq(0x0F.toByte, 0x87.toByte), mnemonic)

private[instructions] class JumpIfSigned(mnemonic: String)
  extends ShortOrLongRelativeJump(Seq(0x78.toByte), Seq(0x0F.toByte, 0x88.toByte), mnemonic)

private[instructions] class JumpIfNotSigned(mnemonic: String)
  extends ShortOrLongRelativeJump(Seq(0x79.toByte), Seq(0x0F.toByte, 0x8B.toByte), mnemonic)

private[instructions] class JumpIfParity(mnemonic: String)
  extends ShortOrLongRelativeJump(Seq(0x7A.toByte), Seq(0x0F.toByte, 0x8A.toByte), mnemonic)

private[instructions] class JumpIfNotParity(mnemonic: String)
  extends ShortOrLongRelativeJump(Seq(0x7B.toByte), Seq(0x0F.toByte, 0x8B.toByte), mnemonic)

private[instructions] class JumpIfSignedNotEqualsOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(Seq(0x7C.toByte), Seq(0x0F.toByte, 0x8C.toByte), mnemonic)

private[instructions] class JumpIfSignedEqualsOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(Seq(0x7D.toByte), Seq(0x0F.toByte, 0x8D.toByte), mnemonic)

private[instructions] class JumpIfZeroAndSignedNotEqualsOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(Seq(0x7E.toByte), Seq(0x0F.toByte, 0x8E.toByte), mnemonic)

private[instructions] class JumpIfNotZeroAndSignedEqualsOverflow(mnemonic: String)
  extends ShortOrLongRelativeJump(Seq(0x7F.toByte), Seq(0x0F.toByte, 0x8F.toByte), mnemonic)

private[instructions] class JumpIfCountZero
  extends ShortRelativeJump(Seq(0xE3.toByte), "jcx") with ShortJumpApply

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