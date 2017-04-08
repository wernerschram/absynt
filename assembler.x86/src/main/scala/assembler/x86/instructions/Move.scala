package assembler.x86.instructions

import assembler.Label
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.{MemoryAddress, MemoryLocation}
import assembler.x86.operands.{ImmediateValue, ModRMEncodableOperand, _}
import assembler.x86.operations.{Immediate, ModRMStatic, ModRRMStatic, ModSegmentRMStatic, RegisterEncoded, ReversedOperands, Static, X86Operation, MemoryLocation => MemoryLocationOperation}

object Move {

  implicit val mnemonic = "mov"

  def apply(source: ModRMEncodableOperand, destination: SegmentRegister)(implicit label: Label, processorMode: ProcessorMode) =
    RM16ToSReg(destination, source)

  private def RM16ToSReg(operand1: SegmentRegister, operand2: ModRMEncodableOperand)(implicit label: Label, processorMode: ProcessorMode) =
    new ModSegmentRMStatic(label, operand1, operand2, 0x8E.toByte :: Nil, mnemonic) with ReversedOperands

  def apply(source: SegmentRegister, destination: ModRMEncodableOperand)(implicit label: Label, processorMode: ProcessorMode) =
    SRegToRM16(source, destination)

  private def SRegToRM16(operand1: SegmentRegister, operand2: ModRMEncodableOperand)(implicit label: Label, processorMode: ProcessorMode) =
    new ModSegmentRMStatic(label, operand1, operand2, 0x8C.toByte :: Nil, mnemonic)

  def apply(source: ByteRegister, destination: ByteRegister)(implicit label: Label, processorMode: ProcessorMode): X86Operation = {
    assume(!(source.isInstanceOf[GeneralPurposeRexRegister] && destination.isInstanceOf[HighByteRegister]))
    assume(!(source.isInstanceOf[HighByteRegister] && destination.isInstanceOf[GeneralPurposeRexRegister]))
    apply(source, destination.asInstanceOf[ModRMEncodableOperand])
  }

  def apply(source: ByteRegister, destination: ModRMEncodableOperand)(implicit label: Label, processorMode: ProcessorMode): X86Operation =
    (source, destination) match {
      case (Register.AL, destination: MemoryAddress) =>
        ALToMOffs8(destination)
      case (source: ByteRegister, destination: ModRMEncodableOperand) =>
        R8ToRM8(source, destination)
    }

  private def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand)(implicit label: Label, processorMode: ProcessorMode) =
    new ModRRMStatic[ByteRegister](label, operand1, operand2, 0x88.toByte :: Nil, mnemonic)

  private def ALToMOffs8(memoryLocation: MemoryLocation)(implicit label: Label, processorMode: ProcessorMode) =
    new Static(label, 0xA2.toByte :: Nil, mnemonic) with MemoryLocationOperation {
      override def operands: List[Operand] = Register.AL :: super.operands

      override val location: MemoryLocation = memoryLocation

      override def operandSize = ValueSize.Byte
    }

  def apply(source: WideRegister, destination: WideRegister)(implicit label: Label, processorMode: ProcessorMode): X86Operation =
    apply(source, destination.asInstanceOf[ModRMEncodableOperand])

  def apply(source: WideRegister, destination: ModRMEncodableOperand)(implicit label: Label, processorMode: ProcessorMode): X86Operation =
    (source, destination) match {
      case (Register.AX, destination: MemoryAddress) =>
        AXToMOffs16(destination)
      case (Register.EAX, destination: MemoryAddress) =>
        EAXToMOffs32(destination)
      case (Register.RAX, destination: MemoryAddress) =>
        RAXToMOffs64(destination)
      case (source: WideRegister, destination: ModRMEncodableOperand) =>
        R16ToRM16(source, destination)
    }

  private def R16ToRM16(operand1: WideRegister, operand2: ModRMEncodableOperand)(implicit label: Label, processorMode: ProcessorMode) =
    new ModRRMStatic[WideRegister](label, operand1, operand2, 0x89.toByte :: Nil, mnemonic)

  private def AXToMOffs16(memoryLocation: MemoryLocation)(implicit label: Label, processorMode: ProcessorMode) =
    new Static(label, 0xA3.toByte :: Nil, mnemonic) with MemoryLocationOperation {
      override def operands: List[Operand] = Register.AX :: super.operands

      override val location: MemoryLocation = memoryLocation

      override def operandSize = ValueSize.Word
    }

  private def EAXToMOffs32(memoryLocation: MemoryLocation)(implicit label: Label, processorMode: ProcessorMode) =
    new Static(label, 0xA3.toByte :: Nil, mnemonic) with MemoryLocationOperation {
      override def operands: List[Operand] = Register.EAX :: super.operands

      override val location: MemoryLocation = memoryLocation

      override def operandSize = ValueSize.DoubleWord
    }

  private def RAXToMOffs64(memoryLocation: MemoryLocation)(implicit label: Label, processorMode: ProcessorMode) =
    new Static(label, 0xA3.toByte :: Nil, mnemonic) with MemoryLocationOperation {
      override def operands: List[Operand] = Register.RAX :: super.operands

      override val location: MemoryLocation = memoryLocation

      override def operandSize = ValueSize.QuadWord
    }

  def apply(source: ModRMEncodableOperand, destination: ByteRegister)(implicit label: Label, processorMode: ProcessorMode): ReversedOperands =
    (source, destination) match {
      case (source: MemoryAddress, Register.AL) =>
        MOffs8ToAL(source)
      case (source: ModRMEncodableOperand, destination: ByteRegister) =>
        RM8ToR8(destination, source)
    }

  private def RM8ToR8(operand1: ByteRegister, operand2: ModRMEncodableOperand)(implicit label: Label, processorMode: ProcessorMode) =
    new ModRRMStatic[ByteRegister](label, operand1, operand2, 0x8A.toByte :: Nil, mnemonic) with ReversedOperands

  private def MOffs8ToAL(memoryLocation: MemoryLocation)(implicit label: Label, processorMode: ProcessorMode) =
    new Static(label, 0xA0.toByte :: Nil, mnemonic) with MemoryLocationOperation with ReversedOperands {
      override def operands: List[Operand] = Register.AL :: super.operands

      override val location: MemoryLocation = memoryLocation

      override def operandSize = ValueSize.Byte
    }

  def apply(source: ModRMEncodableOperand, destination: WideRegister)(implicit label: Label, processorMode: ProcessorMode): ReversedOperands =
    (source, destination) match {
      case (source: MemoryAddress, Register.AX) =>
        MOffs16ToAX(source)
      case (source: MemoryAddress, Register.EAX) =>
        MOffs32ToEAX(source)
      case (source: MemoryAddress, Register.RAX) =>
        MOffs64ToRAX(source)
      case (source: ModRMEncodableOperand, destination: WideRegister) =>
        RM16ToR16(destination, source)
    }

  private def RM16ToR16(operand1: WideRegister, operand2: ModRMEncodableOperand)(implicit label: Label, processorMode: ProcessorMode) =
    new ModRRMStatic[WideRegister](label, operand1, operand2, 0x8B.toByte :: Nil, mnemonic) with ReversedOperands

  private def MOffs16ToAX(memoryLocation: MemoryLocation)(implicit label: Label, processorMode: ProcessorMode) =
    new Static(label, 0xA1.toByte :: Nil, mnemonic) with MemoryLocationOperation with ReversedOperands {
      override def operands: List[Operand] = Register.AX :: super.operands

      override val location: MemoryLocation = memoryLocation

      override def operandSize = ValueSize.Word
    }

  private def MOffs32ToEAX(memoryLocation: MemoryLocation)(implicit label: Label, processorMode: ProcessorMode) =
    new Static(label, 0xA1.toByte :: Nil, mnemonic) with MemoryLocationOperation with ReversedOperands {
      override def operands: List[Operand] = Register.EAX :: super.operands

      override val location: MemoryLocation = memoryLocation

      override def operandSize = ValueSize.DoubleWord
    }

  private def MOffs64ToRAX(memoryLocation: MemoryLocation)(implicit label: Label, processorMode: ProcessorMode) =
    new Static(label, 0xA1.toByte :: Nil, mnemonic) with MemoryLocationOperation with ReversedOperands {
      override def operands: List[Operand] = Register.RAX :: super.operands

      override val location: MemoryLocation = memoryLocation

      override def operandSize = ValueSize.QuadWord
    }

  def apply(source: ImmediateValue, destination: ByteRegister)(implicit label: Label, processorMode: ProcessorMode) =
    Imm8ToR8(destination, source)

  private def Imm8ToR8(register: ByteRegister, immediateValue: ImmediateValue)(implicit label: Label, processorMode: ProcessorMode) =
    new RegisterEncoded[ByteRegister](label, register, 0xB0.toByte :: Nil, mnemonic) with Immediate with ReversedOperands {
      override def immediate: ImmediateValue = immediateValue
    }

  def apply(source: ImmediateValue, destination: WideRegister)(implicit label: Label, processorMode: ProcessorMode) =
    Imm16ToR16(destination, source)

  private def Imm16ToR16(register: WideRegister, immediateValue: ImmediateValue)(implicit label: Label, processorMode: ProcessorMode) =
    new RegisterEncoded[WideRegister](label, register, 0xB8.toByte :: Nil, mnemonic) with Immediate with ReversedOperands {
      override def immediate: ImmediateValue = immediateValue
    }

  def apply(source: ImmediateValue, destination: ModRMEncodableOperand)(implicit label: Label, processorMode: ProcessorMode): ModRMStatic
    with Immediate with ReversedOperands =
    source.operandByteSize match {
      case ValueSize.Byte =>
        Imm8ToRM8(destination, source)
      case _ =>
        Imm16ToRM16(destination, source)
    }

  private def Imm8ToRM8(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit label: Label, processorMode: ProcessorMode) =
    new ModRMStatic(label, operand, 0xC6.toByte :: Nil, 0, mnemonic) with Immediate with ReversedOperands {
      override def immediate: ImmediateValue = immediateValue
    }

  private def Imm16ToRM16(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit label: Label, processorMode: ProcessorMode) =
    new ModRMStatic(label, operand, 0xC7.toByte :: Nil, 0, mnemonic) with Immediate with ReversedOperands {
      override def immediate: ImmediateValue = immediateValue
    }
}
