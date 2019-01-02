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

  def apply(source: ModRMEncodableOperand, destination: SegmentRegister)(implicit processorMode: ProcessorMode): ModSegmentRM =
    RM16ToSReg(destination, source)

  private def RM16ToSReg(operand1: SegmentRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModSegmentRM(operand1, operand2, 0x8E.toByte :: Nil, mnemonic, source) with NoDisplacement with NoImmediate

  def apply(source: SegmentRegister, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode): ModSegmentRM =
    SRegToRM16(source, destination)

  private def SRegToRM16(operand1: SegmentRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModSegmentRM(operand1, operand2, 0x8C.toByte :: Nil, mnemonic, destination) with NoDisplacement with NoImmediate

  def apply(source: ByteRegister, destination: ByteRegister)(implicit processorMode: ProcessorMode): X86Operation = {
    assume(!(source.isInstanceOf[GeneralPurposeRexRegister] && destination.isInstanceOf[HighByteRegister]))
    assume(!(source.isInstanceOf[HighByteRegister] && destination.isInstanceOf[GeneralPurposeRexRegister]))
    apply(source, destination.asInstanceOf[ModRMEncodableOperand with ByteSize])
  }

  def apply[Size<:ValueSize](source: ByteRegister, destination: ModRMEncodableOperand with Size)(implicit processorMode: ProcessorMode): X86Operation =
    (source, destination) match {
      case (Register.AL, destination: MemoryAddress with ByteSize) =>
        ALToMOffs8(destination)
      case (source: ByteRegister, destination: ModRMEncodableOperand) =>
        R8ToRM8(source, destination)
    }

  private def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRM[ByteRegister](operand1, operand2, 0x88.toByte :: Nil, mnemonic, destination) with NoDisplacement with NoImmediate

  private def ALToMOffs8(memoryLocation: MemoryLocation with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xA2.toByte :: Nil, mnemonic) with MemoryLocationOperation[ByteSize] with NoImmediate {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.AL, source))

      override val location: MemoryLocation with ByteSize = memoryLocation

      override def offsetOrder: OperandOrder = destination
    }

  def apply(source: WideRegister, destination: WideRegister)(implicit processorMode: ProcessorMode): X86Operation =
    apply(source, destination.asInstanceOf[ModRMEncodableOperand with WideSize])

  def apply[Size<:WideSize](source: WideRegister, destination: ModRMEncodableOperand with Size)(implicit processorMode: ProcessorMode): X86Operation =
    (source, destination) match {
      case (accumulator: AccumulatorRegister, destination: MemoryAddress) =>
        AXToMOffs16(accumulator, destination)
      case (source: WideRegister, destination: ModRMEncodableOperand) =>
        R16ToRM16(source, destination)
    }

  private def R16ToRM16(operand1: WideRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRM[WideRegister](operand1, operand2, 0x89.toByte :: Nil, mnemonic, destination) with NoDisplacement with NoImmediate

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

  private def RM8ToR8(operand1: ByteRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRM[ByteRegister](operand1, operand2, 0x8A.toByte :: Nil, mnemonic, source) with NoDisplacement with NoImmediate

  private def MOffs8ToAL(memoryLocation: MemoryLocation with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xA0.toByte :: Nil, mnemonic) with MemoryLocationOperation[ByteSize] with NoImmediate {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.AL, destination))

      override val location: MemoryLocation with ByteSize = memoryLocation

      override def offsetOrder: OperandOrder = source
    }

  def apply(source: ModRMEncodableOperand with WideSize, destination: WideRegister)(implicit processorMode: ProcessorMode): X86Operation =
    (source, destination) match {
      case (source: MemoryAddress with WideSize, accumulator: AccumulatorRegister) =>
        MOffs16ToAX(source, accumulator)
      case (source: ModRMEncodableOperand, destination: WideRegister) =>
        RM16ToR16(destination, source)
    }

  private def RM16ToR16(operand1: WideRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRRM[WideRegister](operand1, operand2, 0x8B.toByte :: Nil, mnemonic, source) with NoDisplacement with NoImmediate

  private def MOffs16ToAX[Size<:WideSize](memoryLocation: MemoryLocation with Size, accumulatorRegister: AccumulatorRegister)(implicit processorMode: ProcessorMode) =
    new Static(0xA1.toByte :: Nil, mnemonic) with MemoryLocationOperation[Size] with NoImmediate {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(accumulatorRegister, destination))

      override val location: MemoryLocation with Size = memoryLocation

      override def offsetOrder: OperandOrder = source
    }

  def apply(source: ImmediateValue with ByteSize, destination: ByteRegister)(implicit processorMode: ProcessorMode): RegisterEncoded[ByteRegister] with Immediate[ByteSize] =
    Imm8ToR8(destination, source)

  private def Imm8ToR8(register: ByteRegister, immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new RegisterEncoded[ByteRegister](register, Seq(0xB0.toByte), mnemonic) with NoDisplacement with Immediate[ByteSize] {
      override def immediate: ImmediateValue with ByteSize = immediateValue

      override def immediateOrder: OperandOrder = source

      override def registerOrder: OperandOrder = destination
    }

  def apply(source: ImmediateValue with WideSize, destination: WideRegister)(implicit processorMode: ProcessorMode): RegisterEncoded[WideRegister] with Immediate[WideSize] =
    Imm16ToR16(destination, source)

  private def Imm16ToR16(register: WideRegister, immediateValue: ImmediateValue with WideSize)(implicit processorMode: ProcessorMode) =
    new RegisterEncoded[WideRegister](register, Seq(0xB8.toByte), mnemonic) with NoDisplacement with Immediate[WideSize] {
      assume(register sizeEquals immediateValue)
      override def immediate: ImmediateValue with WideSize = immediateValue

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

  def apply(source: ImmediateValue, destination: ModRMEncodableOperand)(implicit processorMode: ProcessorMode): X86Operation =
    source match {
      case b: ByteSize =>
        Imm8ToRM8(destination, b)
      case w: WideSize =>
        Imm16ToRM16(destination, w)
    }

  private def Imm8ToRM8(operand: ModRMEncodableOperand, immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0xC6.toByte :: Nil, 0, mnemonic, destination) with NoDisplacement with Immediate[ByteSize] {
      override def immediate: ImmediateValue with ByteSize = immediateValue

      override def immediateOrder: OperandOrder = source
    }

  private def Imm16ToRM16[Size<:WideSize](operand: ModRMEncodableOperand, immediateValue: ImmediateValue with Size)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0xC7.toByte :: Nil, 0, mnemonic, destination) with NoDisplacement with Immediate[Size] {
      override def immediate: ImmediateValue with Size = immediateValue

      override def immediateOrder: OperandOrder = source
    }
}
