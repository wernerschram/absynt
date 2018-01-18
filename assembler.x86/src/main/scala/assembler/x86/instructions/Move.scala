package assembler.x86.instructions

import assembler._
import assembler.resource.{AbsoluteReference, UnlabeledEncodable}
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess._
import assembler.x86.operands.{ImmediateValue, ModRMEncodableOperand, _}
import assembler.x86.operations.{Immediate, ModRMStatic, ModRRMStatic, ModSegmentRMStatic, RegisterEncoded, ReversedOperands, Static, X86Operation, MemoryLocation => MemoryLocationOperation}

object Move {

  implicit val mnemonic: String = "mov"

  def apply(source: ModRMEncodableOperand, destination: SegmentRegister)(implicit processorMode: ProcessorMode): ModSegmentRMStatic with ReversedOperands =
    RM16ToSReg(destination, source)

  private def RM16ToSReg(operand1: SegmentRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModSegmentRMStatic(operand1, operand2, 0x8E.toByte :: Nil, mnemonic) with ReversedOperands

  def apply(source: SegmentRegister, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode): ModSegmentRMStatic =
    SRegToRM16(source, destination)

  private def SRegToRM16(operand1: SegmentRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModSegmentRMStatic(operand1, operand2, 0x8C.toByte :: Nil, mnemonic)

  def apply(source: ByteRegister, destination: ByteRegister)(implicit processorMode: ProcessorMode): X86Operation = {
    assume(!(source.isInstanceOf[GeneralPurposeRexRegister] && destination.isInstanceOf[HighByteRegister]))
    assume(!(source.isInstanceOf[HighByteRegister] && destination.isInstanceOf[GeneralPurposeRexRegister]))
    apply(source, destination.asInstanceOf[ModRMEncodableOperand])
  }

  def apply(source: ByteRegister, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode): X86Operation =
    (source, destination) match {
      case (Register.AL, destination: MemoryAddress) =>
        ALToMOffs8(destination)
      case (source: ByteRegister, destination: ModRMEncodableOperand) =>
        R8ToRM8(source, destination)
    }

  private def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStatic[ByteRegister](operand1, operand2, 0x88.toByte :: Nil, mnemonic)

  private def ALToMOffs8(memoryLocation: MemoryLocation)(implicit processorMode: ProcessorMode) =
    new Static(0xA2.toByte :: Nil, mnemonic) with MemoryLocationOperation {
      override def operands: Seq[Operand] = Register.AL +: super.operands

      override val location: MemoryLocation = memoryLocation

      override def operandSize: OperandSize = ValueSize.Byte
    }

  def apply(source: WideRegister, destination: WideRegister)(implicit processorMode: ProcessorMode): X86Operation =
    apply(source, destination.asInstanceOf[ModRMEncodableOperand])

  def apply(source: WideRegister, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode): X86Operation =
    (source, destination) match {
      case (accumulator: AccumulatorRegister, destination: MemoryAddress) =>
        AXToMOffs16(accumulator, destination)
      case (source: WideRegister, destination: ModRMEncodableOperand) =>
        R16ToRM16(source, destination)
    }

  private def R16ToRM16(operand1: WideRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStatic[WideRegister](operand1, operand2, 0x89.toByte :: Nil, mnemonic)

  private def AXToMOffs16(accumulatorRegister: AccumulatorRegister, memoryLocation: MemoryLocation)(implicit processorMode: ProcessorMode) =
    new Static(0xA3.toByte :: Nil, mnemonic) with MemoryLocationOperation {
      override def operands: Seq[Operand] = accumulatorRegister +: super.operands

      override val location: MemoryLocation = memoryLocation

      override def operandSize: OperandSize = accumulatorRegister.operandByteSize
    }

  def apply(source: ModRMEncodableOperand, destination: ByteRegister)(implicit processorMode: ProcessorMode): ReversedOperands =
    (source, destination) match {
      case (source: MemoryAddress, Register.AL) =>
        MOffs8ToAL(source)
      case (source: ModRMEncodableOperand, destination: ByteRegister) =>
        RM8ToR8(destination, source)
    }

  private def RM8ToR8(operand1: ByteRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStatic[ByteRegister](operand1, operand2, 0x8A.toByte :: Nil, mnemonic) with ReversedOperands

  private def MOffs8ToAL(memoryLocation: MemoryLocation)(implicit processorMode: ProcessorMode) =
    new Static(0xA0.toByte :: Nil, mnemonic) with MemoryLocationOperation with ReversedOperands {
      override def operands: Seq[Operand] = Register.AL +: super.operands

      override val location: MemoryLocation = memoryLocation

      override def operandSize: OperandSize = ValueSize.Byte
    }

  def apply(source: ModRMEncodableOperand, destination: WideRegister)(implicit processorMode: ProcessorMode): ReversedOperands =
    (source, destination) match {
      case (source: MemoryAddress, accumulator: AccumulatorRegister) =>
        MOffs16ToAX(source, accumulator)
      case (source: ModRMEncodableOperand, destination: WideRegister) =>
        RM16ToR16(destination, source)
    }

  private def RM16ToR16(operand1: WideRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRMStatic[WideRegister](operand1, operand2, 0x8B.toByte :: Nil, mnemonic) with ReversedOperands

  private def MOffs16ToAX(memoryLocation: MemoryLocation, accumulatorRegister: AccumulatorRegister)(implicit processorMode: ProcessorMode) =
    new Static(0xA1.toByte :: Nil, mnemonic) with MemoryLocationOperation with ReversedOperands {
      override def operands: Seq[Operand] = accumulatorRegister +: super.operands

      override val location: MemoryLocation = memoryLocation

      override def operandSize: OperandSize = accumulatorRegister.operandByteSize
    }

  def apply(source: ImmediateValue, destination: ByteRegister)(implicit processorMode: ProcessorMode): RegisterEncoded[ByteRegister] with Immediate with ReversedOperands =
    Imm8ToR8(destination, source)

  private def Imm8ToR8(register: ByteRegister, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new RegisterEncoded[ByteRegister](register, Seq(0xB0.toByte), mnemonic) with Immediate with ReversedOperands {
      assume(register.operandByteSize == immediateValue.operandByteSize)
      override def immediate: ImmediateValue = immediateValue
    }

  def apply(source: ImmediateValue, destination: WideRegister)(implicit processorMode: ProcessorMode): RegisterEncoded[WideRegister] with Immediate with ReversedOperands =
    Imm16ToR16(destination, source)

  private def Imm16ToR16(register: WideRegister, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new RegisterEncoded[WideRegister](register, Seq(0xB8.toByte), mnemonic) with Immediate with ReversedOperands {
      assume(register.operandByteSize == immediateValue.operandByteSize)
      override def immediate: ImmediateValue = immediateValue
    }

  def forLabel[OffsetType<:X86Offset with AbsoluteOffset](targetLabel: Label, register: WideRegister)
              (implicit processorMode: ProcessorMode): AbsoluteReference = {

    new AbsoluteReference(targetLabel) {
      private val size = processorMode match {
          // prefixes + opcode + immediate
        case (ProcessorMode.Real) => 0 + 1 + 2
        case (ProcessorMode.Protected) => 0 + 1 + 4
        case (ProcessorMode.Long) => 1 + 1 + 8
      }

      override def encodableForDistance(distance: Int): UnlabeledEncodable =
       (processorMode, register) match {
          case (ProcessorMode.Real | ProcessorMode.Protected, _: GeneralPurposeRexRegister) =>
            throw new AssertionError
          case (ProcessorMode.Real, _) =>
            Imm16ToR16(register, distance.toShort)
          case (ProcessorMode.Protected, _: DoubleWordRegister) =>
            Imm16ToR16(register, distance)
          case (ProcessorMode.Long, _: QuadWordRegister) =>
            Imm16ToR16(register, distance.toLong)
          case _ =>
            throw new AssertionError
       }

      override def sizeForDistance(distance: Int): Int = size

      override def possibleSizes: Set[Int] = Set(size)
    }
  }

  def apply(source: ImmediateValue, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode): ModRMStatic
    with Immediate with ReversedOperands =
    source.operandByteSize match {
      case ValueSize.Byte =>
        Imm8ToRM8(destination, source)
      case _ =>
        Imm16ToRM16(destination, source)
    }

  private def Imm8ToRM8(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new ModRMStatic(operand, 0xC6.toByte :: Nil, 0, mnemonic) with Immediate with ReversedOperands {
      override def immediate: ImmediateValue = immediateValue
    }

  private def Imm16ToRM16(operand: ModRMEncodableOperand, immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new ModRMStatic(operand, 0xC7.toByte :: Nil, 0, mnemonic) with Immediate with ReversedOperands {
      override def immediate: ImmediateValue = immediateValue
    }
}
