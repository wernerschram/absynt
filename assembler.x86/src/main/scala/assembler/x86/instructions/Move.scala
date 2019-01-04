package assembler.x86.instructions

import assembler._
import assembler.resource.{AbsoluteReference, UnlabeledEncodable}
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess._
import assembler.x86.operands.{ImmediateValue, ModRMEncodableOperand, _}
import assembler.x86.operations.OperandInfo.OperandOrder._
import assembler.x86.operations.{Immediate, ModRM, ModRRM, ModSegmentRM, NoDisplacement, NoImmediate, OperandInfo, RegisterEncoded, Static, X86Operation, MemoryLocation => MemoryLocationOperation}

object Move {

  implicit val mnemonic: String = "mov"

  def apply[Size<:WideSize](source: ModRMEncodableOperand with Size, destination: SegmentRegister)(implicit processorMode: ProcessorMode): X86Operation =
    RM16ToSReg(destination, source)

  private def RM16ToSReg[Size<:WideSize](operand1: SegmentRegister, operand2: ModRMEncodableOperand with Size)(implicit processorMode: ProcessorMode) =
    new ModSegmentRM(operand1, operand2, 0x8E.toByte :: Nil, mnemonic, source) with NoDisplacement with NoImmediate

  def apply[Size<:WideSize](source: SegmentRegister, destination: ModRMEncodableOperand with Size)(implicit processorMode: ProcessorMode): X86Operation =
    SRegToRM16(source, destination)

  private def SRegToRM16[Size<:WideSize](operand1: SegmentRegister, operand2: ModRMEncodableOperand with Size)(implicit processorMode: ProcessorMode) =
    new ModSegmentRM(operand1, operand2, 0x8C.toByte :: Nil, mnemonic, destination) with NoDisplacement with NoImmediate

  def apply(source: ByteRegister, destination: ByteRegister)(implicit processorMode: ProcessorMode): X86Operation = {
    assume(!(source.isInstanceOf[GeneralPurposeRexRegister] && destination.isInstanceOf[HighByteRegister]))
    assume(!(source.isInstanceOf[HighByteRegister] && destination.isInstanceOf[GeneralPurposeRexRegister]))
    apply(source, destination.asInstanceOf[ModRMEncodableOperand with ByteSize])
  }

  def apply(source: ByteRegister, destination: ModRMEncodableOperand with ByteSize)(implicit processorMode: ProcessorMode): X86Operation =
    (source, destination) match {
      case (Register.AL, destination: MemoryAddress with ByteSize) =>
        ALToMOffs8(destination)
      case _ =>
        R8ToRM8(source, destination)
    }

  private def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand with ByteSize)(implicit processorMode: ProcessorMode) =
    new ModRRM(operand1, operand2, 0x88.toByte :: Nil, mnemonic, destination)

  private def ALToMOffs8(memoryLocation: MemoryLocation with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xA2.toByte :: Nil, mnemonic) with MemoryLocationOperation[ByteSize] with NoImmediate {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.AL, source))

      override val location: MemoryLocation with ByteSize = memoryLocation

      override def offsetOrder: OperandOrder = destination
    }

  def apply[Size<:WideSize](source: GeneralPurposeRegister with Size, destination: GeneralPurposeRegister with Size)(implicit processorMode: ProcessorMode): X86Operation =
    apply(source, destination.asInstanceOf[ModRMEncodableOperand with WideSize])

  def apply[Size<:WideSize](source: GeneralPurposeRegister with Size, destination: ModRMEncodableOperand with Size)(implicit processorMode: ProcessorMode): X86Operation =
    (source, destination) match {
      case (accumulator: AccumulatorRegister, destination: MemoryAddress) =>
        AXToMOffs16(accumulator, destination)
      case (source: WideRegister, destination: ModRMEncodableOperand) =>
        R16ToRM16(source, destination)
    }

  private def R16ToRM16[Size<:WideSize](operand1: GeneralPurposeRegister with Size, operand2: ModRMEncodableOperand with Size)(implicit processorMode: ProcessorMode) =
    new ModRRM(operand1, operand2, 0x89.toByte :: Nil, mnemonic, destination)

  private def AXToMOffs16[Size<:WideSize](accumulatorRegister: AccumulatorRegister, memoryLocation: MemoryLocation with Size)(implicit processorMode: ProcessorMode) =
    new Static(0xA3.toByte :: Nil, mnemonic) with MemoryLocationOperation[Size] with NoImmediate {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(accumulatorRegister, source))

      override val location: MemoryLocation with Size = memoryLocation

      override def offsetOrder: OperandOrder = destination
    }

  def apply(source: ModRMEncodableOperand with ByteSize, destination: ByteRegister)(implicit processorMode: ProcessorMode): X86Operation =
    (source, destination) match {
      case (source: MemoryAddress with ByteSize, Register.AL) =>
        MOffs8ToAL(source)
      case (source: ModRMEncodableOperand with ByteSize, destination: ByteRegister) =>
        RM8ToR8(destination, source)
    }

  private def RM8ToR8(operand1: ByteRegister, operand2: ModRMEncodableOperand with ByteSize)(implicit processorMode: ProcessorMode) =
    new ModRRM(operand1, operand2, 0x8A.toByte :: Nil, mnemonic, source)

  private def MOffs8ToAL(memoryLocation: MemoryLocation with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xA0.toByte :: Nil, mnemonic) with MemoryLocationOperation[ByteSize] with NoImmediate {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.AL, destination))

      override val location: MemoryLocation with ByteSize = memoryLocation

      override def offsetOrder: OperandOrder = source
    }

  def apply[Size<:WideSize](source: ModRMEncodableOperand with Size, destination: GeneralPurposeRegister with Size)(implicit processorMode: ProcessorMode): X86Operation =
    (source, destination) match {
      case (source: MemoryAddress with Size, accumulator: AccumulatorRegister with Size) =>
        MOffs16ToAX(source, accumulator)
      case (source: ModRMEncodableOperand, destination: WideRegister) =>
        RM16ToR16(destination, source)
    }

  private def RM16ToR16[Size<:WideSize](operand1: GeneralPurposeRegister with Size, operand2: ModRMEncodableOperand with Size)(implicit processorMode: ProcessorMode) =
    new ModRRM(operand1, operand2, 0x8B.toByte :: Nil, mnemonic, source)

  private def MOffs16ToAX[Size<:WideSize](memoryLocation: MemoryLocation with Size, accumulatorRegister: AccumulatorRegister with Size)(implicit processorMode: ProcessorMode) =
    new Static(0xA1.toByte :: Nil, mnemonic) with MemoryLocationOperation[Size] with NoImmediate {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(accumulatorRegister, destination))

      override val location: MemoryLocation with Size = memoryLocation

      override def offsetOrder: OperandOrder = source
    }

  def apply(source: ImmediateValue with ByteSize, destination: ByteRegister)(implicit processorMode: ProcessorMode): X86Operation =
    Imm8ToR8(destination, source)

  private def Imm8ToR8(register: ByteRegister, immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new RegisterEncoded[ByteSize](register, Seq(0xB0.toByte), mnemonic) with NoDisplacement with Immediate[ByteSize] {
      override def immediate: ImmediateValue with ByteSize = immediateValue

      override def immediateOrder: OperandOrder = source

      override def registerOrder: OperandOrder = destination
    }

  def apply[Size<:WideSize](source: ImmediateValue with Size, destination: GeneralPurposeRegister with Size)(implicit processorMode: ProcessorMode): X86Operation =
    Imm16ToR16(destination, source)

  private def Imm16ToR16[Size<:WideSize](register: GeneralPurposeRegister with Size, immediateValue: ImmediateValue with Size)(implicit processorMode: ProcessorMode) =
    new RegisterEncoded[Size](register, Seq(0xB8.toByte), mnemonic) with NoDisplacement with Immediate[Size] {
      assume(register sizeEquals immediateValue)
      override def immediate: ImmediateValue with Size = immediateValue

      override def immediateOrder: OperandOrder = source

      override def registerOrder: OperandOrder = destination
    }

  def forLabel(targetLabel: Label, register: WideRegister)
              (implicit processorMode: ProcessorMode): AbsoluteReference = {

    new AbsoluteReference(targetLabel) {
      private val size = processorMode match {
          // prefixes + opcode + immediate
        case ProcessorMode.Real => 0 + 1 + 2
        case ProcessorMode.Protected => 0 + 1 + 4
        case ProcessorMode.Long => 1 + 1 + 8
      }

      override def encodableForDistance(distance: Int): UnlabeledEncodable =
       (processorMode, register) match {
          case (ProcessorMode.Real | ProcessorMode.Protected, _: GeneralPurposeRexRegister) =>
            throw new AssertionError
          case (ProcessorMode.Real, _) =>
            Imm16ToR16(register, distance.toShort)
          case (ProcessorMode.Protected, _: DoubleWordSize) =>
            Imm16ToR16(register, distance)
          case (ProcessorMode.Long, _: QuadWordSize) =>
            Imm16ToR16(register, distance.toLong)
          case _ =>
            throw new AssertionError
       }

      override def sizeForDistance(distance: Int): Int = size

      override def possibleSizes: Set[Int] = Set(size)
    }
  }

  def apply[Size<:ValueSize](source: ImmediateValue with Size, destination: ModRMEncodableOperand with Size)(implicit processorMode: ProcessorMode): X86Operation =
    (source, destination) match {
      case (s: ImmediateValue with ByteSize, d: ModRMEncodableOperand with ByteSize) =>
        Imm8ToRM8(d, s)
      case (s: ImmediateValue with WideSize, d: ModRMEncodableOperand with WideSize) =>
        Imm16ToRM16(d, s)
    }

  private def Imm8ToRM8(operand: ModRMEncodableOperand with ByteSize, immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0xC6.toByte :: Nil, 0, mnemonic, destination) with NoDisplacement with Immediate[ByteSize] {
      override def immediate: ImmediateValue with ByteSize = immediateValue

      override def immediateOrder: OperandOrder = source
    }

  private def Imm16ToRM16[OperandSize<:WideSize, ImmediateSize <: WideSize](operand: ModRMEncodableOperand with OperandSize, immediateValue: ImmediateValue with ImmediateSize)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0xC7.toByte :: Nil, 0, mnemonic, destination) with NoDisplacement with Immediate[ImmediateSize] {
      override def immediate: ImmediateValue with ImmediateSize = immediateValue

      override def immediateOrder: OperandOrder = source
    }
}
