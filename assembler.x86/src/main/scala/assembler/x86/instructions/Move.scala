package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands.ImmediateValue
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.memoryaccess.MemoryAddress
import assembler.x86.operands._
import assembler.x86.operands.memoryaccess.MemoryLocation
import assembler.x86.operations.Immediate
import assembler.x86.operations.ModRMStaticOperation
import assembler.x86.operations.ReversedOperands
import assembler.x86.operations.ReversedOperands
import assembler.x86.operations.ModRRMStaticOperation
import assembler.x86.operations.ModSegmentRMStaticOperation
import assembler.x86.ParameterPosition
import assembler.x86.operations.RegisterEncoded
import assembler.x86.operations.Static
import assembler.x86.operations.{ MemoryLocation => MemoryLocationOperation }
import assembler.x86.operations.X86Operation

object Move {

  implicit val mnemonic = "mov"

  private def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStaticOperation[ByteRegister](operand1, operand2, 0x88.toByte :: Nil, mnemonic)

  private def R16ToRM16(operand1: WideRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStaticOperation[WideRegister](operand1, operand2, 0x89.toByte :: Nil, mnemonic)

  private def RM8ToR8(operand1: ByteRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStaticOperation[ByteRegister](operand1, operand2, 0x8A.toByte :: Nil, mnemonic) with ReversedOperands

  private def RM16ToR16(operand1: WideRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStaticOperation[WideRegister](operand1, operand2, 0x8B.toByte :: Nil, mnemonic) with ReversedOperands

  private def SRegToRM16(operand1: SegmentRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModSegmentRMStaticOperation(operand1, operand2, 0x8C.toByte :: Nil, mnemonic)

  private def RM16ToSReg(operand1: SegmentRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModSegmentRMStaticOperation(operand1, operand2, 0x8E.toByte :: Nil, mnemonic) with ReversedOperands

  private def MOffs8ToAL(memoryLocation: MemoryLocation)(implicit processorMode: ProcessorMode) =
    new Static(0xA0.toByte :: Nil, mnemonic) with MemoryLocationOperation with ReversedOperands {
      override def operands = Register.AL :: super.operands
      override val location = memoryLocation
      override def operandSize = ValueSize.Byte
    }

  private def MOffs16ToAX(memoryLocation: MemoryLocation)(implicit processorMode: ProcessorMode) =
    new Static(0xA1.toByte :: Nil, mnemonic) with MemoryLocationOperation with ReversedOperands {
      override def operands = Register.AX :: super.operands
      override val location = memoryLocation
      override def operandSize = ValueSize.Word
    }

  private def MOffs32ToEAX(memoryLocation: MemoryLocation)(implicit processorMode: ProcessorMode) =
    new Static(0xA1.toByte :: Nil, mnemonic) with MemoryLocationOperation with ReversedOperands {
      override def operands = Register.EAX :: super.operands
      override val location = memoryLocation
      override def operandSize = ValueSize.DoubleWord
    }

  private def MOffs64ToRAX(memoryLocation: MemoryLocation)(implicit processorMode: ProcessorMode) =
    new Static(0xA1.toByte :: Nil, mnemonic) with MemoryLocationOperation with ReversedOperands {
      override def operands = Register.RAX :: super.operands
      override val location = memoryLocation
      override def operandSize = ValueSize.QuadWord
    }

  private def ALToMOffs8(memoryLocation: MemoryLocation)(implicit processorMode: ProcessorMode) =
    new Static(0xA2.toByte :: Nil, mnemonic) with MemoryLocationOperation {
      override def operands = Register.AL :: super.operands
      override val location = memoryLocation
      override def operandSize = ValueSize.Byte
    }

  private def AXToMOffs16(memoryLocation: MemoryLocation)(implicit processorMode: ProcessorMode) =
    new Static(0xA3.toByte :: Nil, mnemonic) with MemoryLocationOperation {
      override def operands = Register.AX :: super.operands
      override val location = memoryLocation
      override def operandSize = ValueSize.Word
    }

  private def EAXToMOffs32(memoryLocation: MemoryLocation)(implicit processorMode: ProcessorMode) =
    new Static(0xA3.toByte :: Nil, mnemonic) with MemoryLocationOperation {
      override def operands = Register.EAX :: super.operands
      override val location = memoryLocation
      override def operandSize = ValueSize.DoubleWord
    }

  private def RAXToMOffs64(memoryLocation: MemoryLocation)(implicit processorMode: ProcessorMode) =
    new Static(0xA3.toByte :: Nil, mnemonic) with MemoryLocationOperation {
      override def operands = Register.RAX :: super.operands
      override val location = memoryLocation
      override def operandSize = ValueSize.QuadWord
    }

  private def Imm8ToR8(register: ByteRegister, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new RegisterEncoded[ByteRegister](register, 0xB0.toByte :: Nil, mnemonic) with Immediate with ReversedOperands {
      override def immediate = immediateValue
    }

  private def Imm16ToR16(register: WideRegister, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new RegisterEncoded[WideRegister](register, 0xB8.toByte :: Nil, mnemonic) with Immediate with ReversedOperands {
      override def immediate = immediateValue
    }

  private def Imm8ToRM8(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new ModRMStaticOperation(operand, 0xC6.toByte :: Nil, 0, mnemonic) with Immediate with ReversedOperands {
      override def immediate = immediateValue
    }

  private def Imm16ToRM16(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new ModRMStaticOperation(operand, 0xC7.toByte :: Nil, 0, mnemonic) with Immediate with ReversedOperands {
      override def immediate = immediateValue
    }

  def apply(source: ModRMEncodableOperand, destination: SegmentRegister)(implicit processorMode: ProcessorMode) =
    RM16ToSReg(destination, source)

  def apply(source: SegmentRegister, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    SRegToRM16(source, destination)

  def apply(source: ByteRegister, destination: ByteRegister)(implicit processorMode: ProcessorMode): X86Operation = {
    assume(!(source.isInstanceOf[GeneralPurposeRexRegister] && destination.isInstanceOf[HighByteRegister]))
    assume(!(source.isInstanceOf[HighByteRegister] && destination.isInstanceOf[GeneralPurposeRexRegister]))
    apply(source, destination.asInstanceOf[ModRMEncodableOperand])
  }

  def apply(source: WideRegister, destination: WideRegister)(implicit processorMode: ProcessorMode): X86Operation =
    apply(source, destination.asInstanceOf[ModRMEncodableOperand])

  def apply(source: ModRMEncodableOperand, destination: ByteRegister)(implicit processorMode: ProcessorMode) = (source, destination) match {
    case (source: MemoryAddress, Register.AL) =>
      MOffs8ToAL(source)
    case (source: ModRMEncodableOperand, destination: ByteRegister) =>
      RM8ToR8(destination, source)
  }

  def apply(source: ModRMEncodableOperand, destination: WideRegister)(implicit processorMode: ProcessorMode) = (source, destination) match {
    case (source: MemoryAddress, Register.AX) =>
      MOffs16ToAX(source)
    case (source: MemoryAddress, Register.EAX) =>
      MOffs32ToEAX(source)
    case (source: MemoryAddress, Register.RAX) =>
      MOffs64ToRAX(source)
    case (source: ModRMEncodableOperand, destination: WideRegister) =>
      RM16ToR16(destination, source)
  }

  def apply(source: ByteRegister, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) = (source, destination) match {
    case (Register.AL, destination: MemoryAddress) =>
      ALToMOffs8(destination)
    case (source: ByteRegister, destination: ModRMEncodableOperand) =>
      R8ToRM8(source, destination)
  }

  def apply(source: WideRegister, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) = (source, destination) match {
    case (Register.AX, destination: MemoryAddress) =>
      AXToMOffs16(destination)
    case (Register.EAX, destination: MemoryAddress) =>
      EAXToMOffs32(destination)
    case (Register.RAX, destination: MemoryAddress) =>
      RAXToMOffs64(destination)
    case (source: WideRegister, destination: ModRMEncodableOperand) =>
      R16ToRM16(source, destination)
  }

  def apply(source: ImmediateValue, destination: ByteRegister)(implicit processorMode: ProcessorMode) =
    Imm8ToR8(destination, source)
  def apply(source: ImmediateValue, destination: WideRegister)(implicit processorMode: ProcessorMode) =
    Imm16ToR16(destination, source)

  def apply(source: ImmediateValue, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) = source.operandByteSize match {
    case ValueSize.Byte =>
      Imm8ToRM8(destination, source)
    case _ =>
      Imm16ToRM16(destination, source)
  }
}
