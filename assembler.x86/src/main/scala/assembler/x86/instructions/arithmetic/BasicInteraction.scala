package assembler.x86.instructions.arithmetic

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operations.Immediate
import assembler.x86.operations.ModRMStaticOperation
import assembler.x86.operations.ModRRMStaticOperation
import assembler.x86.operations.Static

class BasicInteraction(OpcodeBase: Byte, extensionCode: Byte, implicit val mnemonic: String) {

  private def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStaticOperation[ByteRegister](operand1, operand2, (OpcodeBase+0x00).toByte :: Nil, mnemonic)

  private def R16ToRM16(operand1: WideRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStaticOperation[WideRegister](operand1, operand2, (OpcodeBase+0x01).toByte :: Nil, mnemonic)

  private def RM8ToR8(operand1: ByteRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStaticOperation[ByteRegister](operand1, operand2, (OpcodeBase+0x02).toByte :: Nil, mnemonic)

  private def RM16ToR16(operand1: WideRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStaticOperation[WideRegister](operand1, operand2, (OpcodeBase+0x03).toByte :: Nil, mnemonic)

  private def Imm8ToAL(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) = new Static((OpcodeBase+0x04).toByte :: Nil, mnemonic) with Immediate {
    override val immediate = immediateValue
  }

  private def Imm16ToAX(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) = new Static((OpcodeBase+0x05).toByte :: Nil, mnemonic) with Immediate {
    override val immediate = immediateValue
    override def operandSize = Some(2)
  }

  private def Imm32ToEAX(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) = new Static((OpcodeBase+0x5).toByte :: Nil, mnemonic) with Immediate {
    override val immediate = immediateValue
    override def operandSize = Some(4)
  }

  private def Imm32ToRAX(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) = new Static((OpcodeBase+0x5).toByte :: Nil, mnemonic) with Immediate {
    override val immediate = immediateValue
    override def operandSize = Some(8)
  }

  private def Imm8ToRM8(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new ModRMStaticOperation(operand, 0x80.toByte :: Nil, extensionCode, mnemonic) with Immediate {
    override val immediate = immediateValue
  }

  private def Imm16ToRM16(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new ModRMStaticOperation(operand, 0x81.toByte :: Nil, extensionCode, mnemonic) with Immediate {
    override val immediate = immediateValue
  }

  private def Imm8ToRM16(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new ModRMStaticOperation(operand, 0x83.toByte :: Nil, extensionCode, mnemonic) with Immediate {
    override val immediate = immediateValue
  }

  def apply(immediate: ImmediateValue, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) = (destination, immediate) match {
    case (Register.AL, source: ImmediateValue) if (source.operandByteSize == 1) =>
      Imm8ToAL(source)
    case (Register.AX, source: ImmediateValue) if (source.operandByteSize == 2) =>
      Imm16ToAX(source)
    case (Register.EAX, source: ImmediateValue) if (source.operandByteSize == 4) =>
      Imm32ToEAX(source)
    case (Register.RAX, source: ImmediateValue) if (source.operandByteSize == 4) =>
      Imm32ToRAX(source)
    case (destination: FixedSizeModRMEncodableOperand, source: ImmediateValue) if (source.operandByteSize == 1 && destination.operandByteSize == 1) =>
      Imm8ToRM8(destination, source)
    case (destination: FixedSizeModRMEncodableOperand, source: ImmediateValue) if (source.operandByteSize > 1 && source.operandByteSize < 8 && source.operandByteSize == destination.operandByteSize) =>
      Imm16ToRM16(destination, source)
    case (destination: FixedSizeModRMEncodableOperand, source: ImmediateValue) if (source.operandByteSize == 4 && destination.operandByteSize == 8) =>
      Imm16ToRM16(destination, source)
    case (destination: FixedSizeModRMEncodableOperand, source: ImmediateValue) if (source.operandByteSize == 1 && destination.operandByteSize > 1) =>
      Imm8ToRM16(destination, source)
    case default => throw new AssertionError
  }

  def apply(source: ByteRegister, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    R8ToRM8(source, destination)
  def apply(source: ByteRegister, destination: ByteRegister)(implicit processorMode: ProcessorMode) =
    R8ToRM8(source, destination)
  def apply(source: WideRegister, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    R16ToRM16(source, destination)
  def apply(source: WideRegister, destination: WideRegister)(implicit processorMode: ProcessorMode) =
    R16ToRM16(destination, source)

  def apply(source: ModRMEncodableOperand, destination: ByteRegister)(implicit processorMode: ProcessorMode) =
    RM8ToR8(destination, source)
  def apply(source: ModRMEncodableOperand, destination: WideRegister)(implicit processorMode: ProcessorMode) =
    RM16ToR16(destination, source)

}

object Add extends BasicInteraction(0x00.toByte, 0x00.toByte, "add")
object AddCarry extends BasicInteraction(0x10.toByte, 0x02.toByte, "adc")
object And extends BasicInteraction(0x20.toByte, 0x04.toByte, "and")
object Compare extends BasicInteraction(0x38.toByte, 0x07.toByte, "cmp")
object Or extends BasicInteraction(0x08.toByte, 0x01.toByte, "or")
object Subtract extends BasicInteraction(0x28.toByte, 0x05.toByte, "sub")
object SubtractCarry extends BasicInteraction(0x18.toByte, 0x03.toByte, "sbc")
object Xor extends BasicInteraction(0x30.toByte, 0x06.toByte, "xor")
