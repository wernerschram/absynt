package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operations.OperandInfo.OperandOrder._
import assembler.x86.operations._

class BasicInteraction(OpcodeBase: Byte, extensionCode: Byte, implicit val mnemonic: String) {

  def apply(immediate: ImmediateValue with ByteSize, destination: Register.AL.type)(implicit processorMode: ProcessorMode): X86Operation =
    Imm8ToAL(immediate)

  def apply(immediate: ImmediateValue with WordSize, destination: Register.AX.type)(implicit processorMode: ProcessorMode): X86Operation =
    Imm16ToAX(immediate)

  def apply(immediate: ImmediateValue with DoubleWordSize, destination: Register.EAX.type)(implicit processorMode: ProcessorMode): X86Operation =
    Imm32ToEAX(immediate)

  def apply(immediate: ImmediateValue with DoubleWordSize, destination: Register.RAX.type)(implicit processorMode: ProcessorMode): X86Operation =
    Imm32ToRAX(immediate)

  def apply(immediate: ImmediateValue, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode): X86Operation =
    (immediate, destination) match {
      case (imm: ImmediateValue with ByteSize, d: WideSize) =>
        Imm8ToRM16(d, imm)
      case (imm: ImmediateValue with ByteSize, _) =>
        Imm8ToRM8(destination, imm)
      case (imm: DoubleWordSize, _: QuadWordSize) =>
        Imm16ToRM16(destination, imm)
      case (_: QuadWordSize, _) =>
        throw new AssertionError
      case (_, dest: ValueSize) if !(dest sizeEquals immediate) =>
        throw new AssertionError
      case (imm: WideSize, _) =>
        Imm16ToRM16(destination, imm)
      case _ =>
        throw new AssertionError
    }

  private def Imm8ToAL(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static((OpcodeBase + 0x04).toByte :: Nil, mnemonic) with Immediate {
      override def operands: Set[OperandInfo] = super.operands + OperandInfo.implicitOperand(Register.AL, destination)
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue = immediateValue
    }

  private def Imm16ToAX(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static((OpcodeBase + 0x05).toByte :: Nil, mnemonic) with Immediate {
      override def operands: Set[OperandInfo] = super.operands + OperandInfo.implicitOperand(Register.AX, destination)
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue = immediateValue
    }

  private def Imm32ToEAX(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static((OpcodeBase + 0x5).toByte :: Nil, mnemonic) with Immediate {
      override def operands: Set[OperandInfo] = super.operands + OperandInfo.implicitOperand(Register.EAX, destination)
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue = immediateValue
    }

  private def Imm32ToRAX(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static((OpcodeBase + 0x5).toByte :: Nil, mnemonic) with Immediate {
      override def operands: Set[OperandInfo] = super.operands + OperandInfo.implicitOperand(Register.RAX, destination)
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue = immediateValue
    }

  private def Imm8ToRM8(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new ModRMStatic(operand, 0x80.toByte :: Nil, extensionCode, mnemonic) with Immediate {
      override val immediateOrder: OperandOrder = source
      override val operandRMOrder: OperandOrder = destination
      override val immediate: ImmediateValue = immediateValue
    }

  private def Imm16ToRM16(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new ModRMStatic(operand, 0x81.toByte :: Nil, extensionCode, mnemonic) with Immediate {
      override val immediateOrder: OperandOrder = source
      override val operandRMOrder: OperandOrder = destination
      override val immediate: ImmediateValue = immediateValue
    }

  private def Imm8ToRM16(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new ModRMStatic(operand, 0x83.toByte :: Nil, extensionCode, mnemonic) with Immediate {
      override val immediateOrder: OperandOrder = source
      override val operandRMOrder: OperandOrder = destination
      override val immediate: ImmediateValue = immediateValue
    }

  def apply(source: ByteRegister, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode): ModRRMStatic[ByteRegister] =
    R8ToRM8(source, destination)

  private def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStatic[ByteRegister](operand1, operand2, (OpcodeBase + 0x00).toByte :: Nil, mnemonic) {
      override val operandRMOrder: OperandOrder = destination
    }

  def apply(source: ByteRegister, destination: ByteRegister)(implicit processorMode: ProcessorMode): ModRRMStatic[ByteRegister] =
    R8ToRM8(source, destination)

  def apply(source: WideRegister, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode): ModRRMStatic[WideRegister] =
    R16ToRM16(source, destination)

  private def R16ToRM16(operand1: WideRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStatic[WideRegister](operand1, operand2, (OpcodeBase + 0x01).toByte :: Nil, mnemonic) {
      override val operandRMOrder: OperandOrder = destination
    }

  def apply(source: WideRegister, destination: WideRegister)(implicit processorMode: ProcessorMode): ModRRMStatic[WideRegister] =
    R16ToRM16(destination, source)

  def apply(source: ModRMEncodableOperand, destination: ByteRegister)(implicit processorMode: ProcessorMode): ModRRMStatic[ByteRegister] =
    RM8ToR8(destination, source)

  private def RM8ToR8(operand1: ByteRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStatic[ByteRegister](operand1, operand2, (OpcodeBase + 0x02).toByte :: Nil, mnemonic) {
      override val operandRMOrder: OperandOrder = source
    }

  def apply(source: ModRMEncodableOperand, destination: WideRegister)(implicit processorMode: ProcessorMode): ModRRMStatic[WideRegister] =
    RM16ToR16(destination, source)

  private def RM16ToR16(operand1: WideRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStatic[WideRegister](operand1, operand2, (OpcodeBase + 0x03).toByte :: Nil, mnemonic) {
      override val operandRMOrder: OperandOrder = source
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

  def apply(operand: ModRMEncodableOperand with ValueSize)(implicit processorMode: ProcessorMode): ModRMStatic =
    operand match {
      case o:ByteSize => RM8(o)
      case o:WideSize => RM16(o)
    }

  private def RM8(operand: ModRMEncodableOperand with ByteSize)(implicit processorMode: ProcessorMode) =
    new ModRMStatic(operand, 0xF6.toByte :: Nil, 2, opcode) {
      override val operandRMOrder: OperandOrder = destination
    }

  private def RM16(operand: ModRMEncodableOperand with WideSize)(implicit processorMode: ProcessorMode) =
    new ModRMStatic(operand, 0xF7.toByte :: Nil, 2, opcode) {
      override val operandRMOrder: OperandOrder = destination
    }
}
