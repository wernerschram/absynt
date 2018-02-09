package assembler.x86.instructions

import assembler.x86.operands._
import assembler.x86.operations._
import assembler.x86.{ParameterPosition, ProcessorMode, RexRequirement}
import assembler.x86.operations.OperandInfo.OperandOrder._

class BasicInteraction(OpcodeBase: Byte, extensionCode: Byte, implicit val mnemonic: String) {

  def apply(immediate: ImmediateValue, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode): Immediate =
    (destination, immediate.operandByteSize) match {
      case (Register.AL, ValueSize.Byte) =>
        Imm8ToAL(immediate)
      case (Register.AX, ValueSize.Word) =>
        Imm16ToAX(immediate)
      case (Register.EAX, ValueSize.DoubleWord) =>
        Imm32ToEAX(immediate)
      case (Register.RAX, ValueSize.DoubleWord) =>
        Imm32ToRAX(immediate)
      case (destination: ModRMEncodableOperand with FixedSizeOperand, ValueSize.Byte) if destination.operandByteSize != ValueSize.Byte =>
        Imm8ToRM16(destination, immediate)
      case (destination: ModRMEncodableOperand with FixedSizeOperand, valueSize)
        if valueSize == ValueSize.QuadWord || !(destination.operandByteSize == valueSize ||
          (destination.operandByteSize == ValueSize.QuadWord && valueSize == ValueSize.DoubleWord)) =>
        throw new AssertionError
      case (destination: ModRMEncodableOperand, ValueSize.Byte) =>
        Imm8ToRM8(destination, immediate)
      case (destination: ModRMEncodableOperand, _) =>
        Imm16ToRM16(destination, immediate)
      case _ => throw new AssertionError
    }

  private def Imm8ToAL(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static((OpcodeBase + 0x04).toByte :: Nil, mnemonic) with Immediate {
      override val immediateOrder: OperandOrder = first
      override val immediate: ImmediateValue = immediateValue
    }

  private def Imm16ToAX(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static((OpcodeBase + 0x05).toByte :: Nil, mnemonic) with Immediate {
      override val immediateOrder: OperandOrder = first
      override val immediate: ImmediateValue = immediateValue

      override def operandSize: OperandSize = ValueSize.Word
    }

  private def Imm32ToEAX(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static((OpcodeBase + 0x5).toByte :: Nil, mnemonic) with Immediate {
      override val immediateOrder: OperandOrder = first
      override val immediate: ImmediateValue = immediateValue

      override def operandSize: OperandSize = ValueSize.DoubleWord
    }

  private def Imm32ToRAX(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static((OpcodeBase + 0x5).toByte :: Nil, mnemonic) with Immediate {
      override val immediateOrder: OperandOrder = first
      override val immediate: ImmediateValue = immediateValue

      override def operandSize: OperandSize = ValueSize.QuadWord
    }

  private def Imm8ToRM8(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new ModRMStatic(operand, 0x80.toByte :: Nil, extensionCode, mnemonic) with Immediate {
      override val immediateOrder: OperandOrder = first
      override val operandRMOrder: OperandOrder = second
      override val immediate: ImmediateValue = immediateValue
    }

  private def Imm16ToRM16(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new ModRMStatic(operand, 0x81.toByte :: Nil, extensionCode, mnemonic) with Immediate {
      override val immediateOrder: OperandInfo.OperandOrder.Value = first
      override val operandRMOrder: OperandOrder = second
      override val immediate: ImmediateValue = immediateValue
    }

  private def Imm8ToRM16(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new ModRMStatic(operand, 0x83.toByte :: Nil, extensionCode, mnemonic) with Immediate {
      override val immediateOrder: OperandInfo.OperandOrder.Value = first
      override val operandRMOrder: OperandOrder = second
      override val immediate: ImmediateValue = immediateValue

      override def rexRequirements: Seq[RexRequirement] =
        operand.getRexRequirements(ParameterPosition.NotEncoded) ++ super.rexRequirements
    }

  def apply(source: ByteRegister, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode): ModRRMStatic[ByteRegister] =
    R8ToRM8(source, destination)

  private def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStatic[ByteRegister](operand1, operand2, (OpcodeBase + 0x00).toByte :: Nil, mnemonic) {
      override val operandRMOrder: OperandOrder = second
    }

  def apply(source: ByteRegister, destination: ByteRegister)(implicit processorMode: ProcessorMode): ModRRMStatic[ByteRegister] =
    R8ToRM8(source, destination)

  def apply(source: WideRegister, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode): ModRRMStatic[WideRegister] =
    R16ToRM16(source, destination)

  private def R16ToRM16(operand1: WideRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStatic[WideRegister](operand1, operand2, (OpcodeBase + 0x01).toByte :: Nil, mnemonic) {
      override val operandRMOrder: OperandOrder = second
    }

  def apply(source: WideRegister, destination: WideRegister)(implicit processorMode: ProcessorMode): ModRRMStatic[WideRegister] =
    R16ToRM16(destination, source)

  def apply(source: ModRMEncodableOperand, destination: ByteRegister)(implicit processorMode: ProcessorMode): ModRRMStatic[ByteRegister] =
    RM8ToR8(destination, source)

  private def RM8ToR8(operand1: ByteRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStatic[ByteRegister](operand1, operand2, (OpcodeBase + 0x02).toByte :: Nil, mnemonic) {
      override val operandRMOrder: OperandOrder = first
    }

  def apply(source: ModRMEncodableOperand, destination: WideRegister)(implicit processorMode: ProcessorMode): ModRRMStatic[WideRegister] =
    RM16ToR16(destination, source)

  private def RM16ToR16(operand1: WideRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStatic[WideRegister](operand1, operand2, (OpcodeBase + 0x03).toByte :: Nil, mnemonic) {
      override val operandRMOrder: OperandOrder = first
    }

}

object Add extends BasicInteraction(0x00.toByte, 0x00.toByte, "add")
object AddCarry extends BasicInteraction(0x10.toByte, 0x02.toByte, "adc")
object And extends BasicInteraction(0x20.toByte, 0x04.toByte, "and")
object Compare extends BasicInteraction(0x38.toByte, 0x07.toByte, "cmp")
object Or extends BasicInteraction(0x08.toByte, 0x01.toByte, "or")
object Subtract extends BasicInteraction(0x28.toByte, 0x05.toByte, "sub")
object SubtractCarry extends BasicInteraction(0x18.toByte, 0x03.toByte, "sbc")
object Xor extends BasicInteraction(0x30.toByte, 0x06.toByte, "xor")

object Not {
  implicit val opcode: String = "not"

  def apply(operand: ModRMEncodableOperand with FixedSizeOperand)(implicit processorMode: ProcessorMode): ModRMStatic =
    operand.operandByteSize match {
      case ValueSize.Byte => RM8(operand)
      case _ => RM16(operand)
    }

  private def RM8(operand: ModRMEncodableOperand with FixedSizeOperand)(implicit processorMode: ProcessorMode) =
    new ModRMStatic(operand, 0xF6.toByte :: Nil, 2, opcode) {
      override val operandRMOrder: OperandOrder = first
    }

  private def RM16(operand: ModRMEncodableOperand with FixedSizeOperand)(implicit processorMode: ProcessorMode) =
    new ModRMStatic(operand, 0xF7.toByte :: Nil, 2, opcode) {
      override val operandRMOrder: OperandOrder = first
    }
}
