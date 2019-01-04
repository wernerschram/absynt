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

  def apply(immediate: ImmediateValue with ValueSize, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode): X86Operation =
    (immediate, destination) match {
      case (imm: ImmediateValue with ByteSize, d: ModRMEncodableOperand with WideSize) =>
        Imm8ToRM16(d, imm)
      case (imm: ImmediateValue with ByteSize, _) =>
        Imm8ToRM8(destination, imm)
      case (imm: ImmediateValue with DoubleWordSize, _: ModRMEncodableOperand with QuadWordSize) =>
        Imm16ToRM16(destination, imm)
      case (_: ImmediateValue with QuadWordSize, _) =>
        throw new AssertionError
      case (_, dest: ValueSize) if !(dest sizeEquals immediate) =>
        throw new AssertionError
      case (imm: ImmediateValue with WideSize, _) =>
        Imm16ToRM16(destination, imm)
      case _ =>
        throw new AssertionError
    }

  private def Imm8ToAL(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static((OpcodeBase + 0x04).toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.AL, destination))
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue with ByteSize = immediateValue
    }

  private def Imm16ToAX(immediateValue: ImmediateValue with WordSize)(implicit processorMode: ProcessorMode) =
    new Static((OpcodeBase + 0x05).toByte :: Nil, mnemonic) with NoDisplacement with Immediate[WordSize] {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.AX, destination))
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue with WordSize = immediateValue
    }

  private def Imm32ToEAX(immediateValue: ImmediateValue with DoubleWordSize)(implicit processorMode: ProcessorMode) =
    new Static((OpcodeBase + 0x5).toByte :: Nil, mnemonic) with NoDisplacement with Immediate[DoubleWordSize] {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.EAX, destination))
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue with DoubleWordSize = immediateValue
    }

  private def Imm32ToRAX(immediateValue: ImmediateValue with DoubleWordSize)(implicit processorMode: ProcessorMode) =
    new Static((OpcodeBase + 0x5).toByte :: Nil, mnemonic) with NoDisplacement with Immediate[DoubleWordSize] {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.RAX, destination))
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue with DoubleWordSize = immediateValue
    }

  private def Imm8ToRM8(operand: ModRMEncodableOperand, immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0x80.toByte :: Nil, extensionCode, mnemonic, destination) with NoDisplacement with Immediate[ByteSize] {
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue with ByteSize = immediateValue
    }

  private def Imm16ToRM16[Size<:WideSize](operand: ModRMEncodableOperand, immediateValue: ImmediateValue with Size)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0x81.toByte :: Nil, extensionCode, mnemonic, destination) with NoDisplacement with Immediate[Size] {
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue with Size = immediateValue
    }

  private def Imm8ToRM16(operand: ModRMEncodableOperand, immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0x83.toByte :: Nil, extensionCode, mnemonic, destination) with NoDisplacement with Immediate[ByteSize] {
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue with ByteSize = immediateValue
    }

  def apply(source: ByteRegister, destination: ModRMEncodableOperand with ByteSize)(implicit processorMode: ProcessorMode): X86Operation =
    R8ToRM8(source, destination)

  private def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand with ByteSize)(implicit processorMode: ProcessorMode) =
    new ModRRM(operand1, operand2, (OpcodeBase + 0x00).toByte :: Nil, mnemonic, destination)

  def apply(source: ByteRegister, destination: ByteRegister)(implicit processorMode: ProcessorMode): X86Operation =
    R8ToRM8(source, destination)

  def apply(source: WideRegister, destination: ModRMEncodableOperand with WideSize)(implicit processorMode: ProcessorMode): X86Operation =
    R16ToRM16(source, destination)

  private def R16ToRM16(operand1: WideRegister, operand2: ModRMEncodableOperand with WideSize)(implicit processorMode: ProcessorMode) =
    new ModRRM(operand1, operand2, (OpcodeBase + 0x01).toByte :: Nil, mnemonic, destination)

  def apply(source: WideRegister, destination: WideRegister)(implicit processorMode: ProcessorMode): X86Operation =
    R16ToRM16(destination, source)

  def apply(source: ModRMEncodableOperand with ByteSize, destination: ByteRegister)(implicit processorMode: ProcessorMode): X86Operation =
    RM8ToR8(destination, source)

  private def RM8ToR8(operand1: ByteRegister, operand2: ModRMEncodableOperand with ByteSize)(implicit processorMode: ProcessorMode) =
    new ModRRM(operand1, operand2, (OpcodeBase + 0x02).toByte :: Nil, mnemonic, source)

  def apply[Size<:WideSize](source: ModRMEncodableOperand with Size, destination: GeneralPurposeRegister with Size)(implicit processorMode: ProcessorMode): X86Operation =
    RM16ToR16(destination, source)

  private def RM16ToR16[Size<:WideSize](operand1: GeneralPurposeRegister with Size, operand2: ModRMEncodableOperand with Size)(implicit processorMode: ProcessorMode) =
    new ModRRM(operand1, operand2, (OpcodeBase + 0x03).toByte :: Nil, mnemonic, source)

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

  private def RM16[Size<:WideSize](operand: ModRMEncodableOperand with Size)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0xF7.toByte :: Nil, 2, opcode, destination) with NoDisplacement with NoImmediate
}
