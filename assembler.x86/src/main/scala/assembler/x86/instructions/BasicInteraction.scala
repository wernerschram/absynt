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
    new Static((OpcodeBase + 0x04).toByte :: Nil, mnemonic) with NoDisplacement with Immediate {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.AL, destination))
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue = immediateValue
    }

  private def Imm16ToAX(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static((OpcodeBase + 0x05).toByte :: Nil, mnemonic) with NoDisplacement with Immediate {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.AX, destination))
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue = immediateValue
    }

  private def Imm32ToEAX(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static((OpcodeBase + 0x5).toByte :: Nil, mnemonic) with NoDisplacement with Immediate {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.EAX, destination))
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue = immediateValue
    }

  private def Imm32ToRAX(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static((OpcodeBase + 0x5).toByte :: Nil, mnemonic) with NoDisplacement with Immediate {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.RAX, destination))
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue = immediateValue
    }

  private def Imm8ToRM8(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0x80.toByte :: Nil, extensionCode, mnemonic, destination) with NoDisplacement with Immediate {
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue = immediateValue
    }

  private def Imm16ToRM16(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0x81.toByte :: Nil, extensionCode, mnemonic, destination) with NoDisplacement with Immediate {
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue = immediateValue
    }

  private def Imm8ToRM16(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0x83.toByte :: Nil, extensionCode, mnemonic, destination) with NoDisplacement with Immediate {
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue = immediateValue
    }

  def apply(source: ByteRegister, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode): ModRRM[ByteRegister] =
    R8ToRM8(source, destination)

  private def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRM[ByteRegister](operand1, operand2, (OpcodeBase + 0x00).toByte :: Nil, mnemonic, destination) with NoDisplacement with NoImmediate

  def apply(source: ByteRegister, destination: ByteRegister)(implicit processorMode: ProcessorMode): ModRRM[ByteRegister] =
    R8ToRM8(source, destination)

  def apply(source: WideRegister, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode): ModRRM[WideRegister] =
    R16ToRM16(source, destination)

  private def R16ToRM16(operand1: WideRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRM[WideRegister](operand1, operand2, (OpcodeBase + 0x01).toByte :: Nil, mnemonic, destination) with NoDisplacement with NoImmediate

  def apply(source: WideRegister, destination: WideRegister)(implicit processorMode: ProcessorMode): ModRRM[WideRegister] =
    R16ToRM16(destination, source)

  def apply(source: ModRMEncodableOperand, destination: ByteRegister)(implicit processorMode: ProcessorMode): ModRRM[ByteRegister] =
    RM8ToR8(destination, source)

  private def RM8ToR8(operand1: ByteRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRM[ByteRegister](operand1, operand2, (OpcodeBase + 0x02).toByte :: Nil, mnemonic, source) with NoDisplacement with NoImmediate

  def apply(source: ModRMEncodableOperand, destination: WideRegister)(implicit processorMode: ProcessorMode): ModRRM[WideRegister] =
    RM16ToR16(destination, source)

  private def RM16ToR16(operand1: WideRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRM[WideRegister](operand1, operand2, (OpcodeBase + 0x03).toByte :: Nil, mnemonic, source) with NoDisplacement with NoImmediate

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

  def apply(operand: ModRMEncodableOperand with ValueSize)(implicit processorMode: ProcessorMode): ModRM =
    operand match {
      case o:ByteSize => RM8(o)
      case o:WideSize => RM16(o)
    }

  private def RM8(operand: ModRMEncodableOperand with ByteSize)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0xF6.toByte :: Nil, 2, opcode, destination) with NoDisplacement with NoImmediate

  private def RM16(operand: ModRMEncodableOperand with WideSize)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0xF7.toByte :: Nil, 2, opcode, destination) with NoDisplacement with NoImmediate
}
