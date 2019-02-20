package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operations.OperandInfo.OperandOrder._
import assembler.x86.operations._


object BasicInteraction {
  private def Imm8ToAL(immediateValue: ImmediateValue with ByteSize, opcodeBase: Byte, mnemonic: String)(implicit processorMode: ProcessorMode) =
    new Static((opcodeBase + 0x04).toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Accumulator.LowByte, destination))
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue with ByteSize = immediateValue
    }

  private def Imm16ToAX(immediateValue: ImmediateValue with WordSize, opcodeBase: Byte, mnemonic: String)(implicit processorMode: ProcessorMode) =
    new Static((opcodeBase + 0x05).toByte :: Nil, mnemonic) with NoDisplacement with Immediate[WordSize] {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Accumulator.Word, destination))
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue with WordSize = immediateValue
    }

  private def Imm32ToEAX(immediateValue: ImmediateValue with DoubleWordSize, opcodeBase: Byte, mnemonic: String)(implicit processorMode: ProcessorMode) =
    new Static((opcodeBase + 0x5).toByte :: Nil, mnemonic) with NoDisplacement with Immediate[DoubleWordSize] {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Accumulator.DoubleWord, destination))
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue with DoubleWordSize = immediateValue
    }

  private def Imm32ToRAX(immediateValue: ImmediateValue with DoubleWordSize, opcodeBase: Byte, mnemonic: String)(implicit processorMode: ProcessorMode) =
    new Static((opcodeBase + 0x5).toByte :: Nil, mnemonic) with NoDisplacement with Immediate[DoubleWordSize] {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Accumulator.QuadWord, destination))
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue with DoubleWordSize = immediateValue
    }

  private def Imm8ToRM8(operand: ModRMEncodableOperand with ByteSize, immediateValue: ImmediateValue with ByteSize, extensionCode: Byte, opcodeBase: Byte, mnemonic: String)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0x80.toByte :: Nil, extensionCode, mnemonic, destination) with NoDisplacement with Immediate[ByteSize] {
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue with ByteSize = immediateValue
    }

  private def Imm16ToRM16[Size<:WideSize](operand: ModRMEncodableOperand with Size, immediateValue: ImmediateValue with Size, extensionCode: Byte, opcodeBase: Byte, mnemonic: String)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0x81.toByte :: Nil, extensionCode, mnemonic, destination) with NoDisplacement with Immediate[Size] {
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue with Size = immediateValue
    }

  private def Imm8ToRM16[Size<:WideSize](operand: ModRMEncodableOperand with Size, immediateValue: ImmediateValue with ByteSize, extensionCode: Byte, opcodeBase: Byte, mnemonic: String)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0x83.toByte :: Nil, extensionCode, mnemonic, destination) with NoDisplacement with Immediate[ByteSize] {
      override val immediateOrder: OperandOrder = source
      override val immediate: ImmediateValue with ByteSize = immediateValue
    }

  private def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand with ByteSize, opcodeBase: Byte, mnemonic: String)(implicit processorMode: ProcessorMode) =
    new ModRRM(operand1, operand2, (opcodeBase + 0x00).toByte :: Nil, mnemonic, destination)

  private def R16ToRM16[Size<:WideSize](operand1: GeneralPurposeRegister with Size, operand2: ModRMEncodableOperand with Size, opcodeBase: Byte, mnemonic: String)(implicit processorMode: ProcessorMode) =
    new ModRRM(operand1, operand2, (opcodeBase + 0x01).toByte :: Nil, mnemonic, destination)

  private def RM8ToR8(operand1: ByteRegister, operand2: ModRMEncodableOperand with ByteSize, opcodeBase: Byte, mnemonic: String)(implicit processorMode: ProcessorMode) =
    new ModRRM(operand1, operand2, (opcodeBase + 0x02).toByte :: Nil, mnemonic, source)

  private def RM16ToR16[Size<:WideSize](operand1: GeneralPurposeRegister with Size, operand2: ModRMEncodableOperand with Size, opcodeBase: Byte, mnemonic: String)(implicit processorMode: ProcessorMode) =
    new ModRRM(operand1, operand2, (opcodeBase + 0x03).toByte :: Nil, mnemonic, source)

  private def RM8(operand: ModRMEncodableOperand with ByteSize, mnemonic: String)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0xF6.toByte :: Nil, 2, mnemonic, destination) with NoDisplacement with NoImmediate

  private def RM16[Size<:WideSize](operand: ModRMEncodableOperand with Size, mnemonic: String)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0xF7.toByte :: Nil, 2, mnemonic, destination) with NoDisplacement with NoImmediate



  sealed class I8086BasicInteraction(opcodeBase: Byte, extensionCode: Byte, mnemonic: String) {

    def apply(immediate: ImmediateValue with ByteSize, destination: Accumulator.LowByte.type)(implicit processorMode: ProcessorMode): X86Operation =
      Imm8ToAL(immediate, opcodeBase, mnemonic)

    def apply(immediate: ImmediateValue with WordSize, destination: Accumulator.Word.type)(implicit processorMode: ProcessorMode): X86Operation =
      Imm16ToAX(immediate, opcodeBase, mnemonic)

    def apply[ImmediateSize<:ValueSize, DestinationSize<:ValueSize](immediate: ImmediateValue with ImmediateSize, destination: ModRMEncodableOperand with DestinationSize)(implicit processorMode: ProcessorMode): X86Operation =
      (immediate, destination) match {
        case (imm: ImmediateValue with ByteSize, d: ModRMEncodableOperand with WordSize) =>
          Imm8ToRM16(d, imm, extensionCode, opcodeBase, mnemonic)
        case (imm: ImmediateValue with ByteSize, d: ModRMEncodableOperand with ByteSize) =>
          Imm8ToRM8(d, imm, extensionCode, opcodeBase, mnemonic)
        case (imm: ImmediateValue with WordSize, d: ModRMEncodableOperand with WordSize) =>
          Imm16ToRM16(d, imm, extensionCode, opcodeBase, mnemonic)
        case _ =>
          throw new AssertionError
      }

    def apply(source: ByteRegister, destination: ModRMEncodableOperand with ByteSize)(implicit processorMode: ProcessorMode): X86Operation =
      R8ToRM8(source, destination, opcodeBase, mnemonic)

    def apply(source: ByteRegister, destination: ByteRegister)(implicit processorMode: ProcessorMode): X86Operation =
      R8ToRM8(source, destination, opcodeBase, mnemonic)

    def apply(source: GeneralPurposeRegister with WordSize, destination: ModRMEncodableOperand with WordSize)(implicit processorMode: ProcessorMode): X86Operation =
      R16ToRM16(source, destination, opcodeBase, mnemonic)

    def apply(source: GeneralPurposeRegister with WordSize, destination: GeneralPurposeRegister with WordSize)(implicit processorMode: ProcessorMode): X86Operation =
      R16ToRM16(destination, source, opcodeBase, mnemonic)

    def apply(source: ModRMEncodableOperand with ByteSize, destination: ByteRegister)(implicit processorMode: ProcessorMode): X86Operation =
      RM8ToR8(destination, source, opcodeBase, mnemonic)

    def apply(source: ModRMEncodableOperand with WordSize, destination: GeneralPurposeRegister with WordSize)(implicit processorMode: ProcessorMode): X86Operation =
      RM16ToR16(destination, source, opcodeBase, mnemonic)

  }

  sealed class I386BasicInteraction(opcodeBase: Byte, extensionCode: Byte, mnemonic: String) {

    def apply(immediate: ImmediateValue with ByteSize, destination: Accumulator.LowByte.type)(implicit processorMode: ProcessorMode): X86Operation =
      Imm8ToAL(immediate, opcodeBase, mnemonic)

    def apply(immediate: ImmediateValue with WordSize, destination: Accumulator.Word.type)(implicit processorMode: ProcessorMode): X86Operation =
      Imm16ToAX(immediate, opcodeBase, mnemonic)

    def apply(immediate: ImmediateValue with DoubleWordSize, destination: Accumulator.DoubleWord.type)(implicit processorMode: ProcessorMode): X86Operation =
      Imm32ToEAX(immediate, opcodeBase, mnemonic)

    def apply[ImmediateSize<:DisplacementSize, DestinationSize<:DisplacementSize](immediate: ImmediateValue with ImmediateSize, destination: ModRMEncodableOperand with DestinationSize)(implicit processorMode: ProcessorMode): X86Operation =
      (immediate, destination) match {
        case (imm: ImmediateValue with ByteSize, d: ModRMEncodableOperand with WideSize) =>
          Imm8ToRM16(d, imm, extensionCode, opcodeBase, mnemonic)
        case (imm: ImmediateValue with ByteSize, d: ModRMEncodableOperand with ByteSize) =>
          Imm8ToRM8(d, imm, extensionCode, opcodeBase, mnemonic)
        case (_, dest: ValueSize) if !(dest sizeEquals immediate) =>
          throw new AssertionError
        case (imm: ImmediateValue with WideSize, d: ModRMEncodableOperand with WideSize) =>
          Imm16ToRM16(d, imm, extensionCode, opcodeBase, mnemonic)
        case _ =>
          throw new AssertionError
      }

    def apply(source: ByteRegister, destination: ModRMEncodableOperand with ByteSize)(implicit processorMode: ProcessorMode): X86Operation =
      R8ToRM8(source, destination, opcodeBase, mnemonic)

    def apply(source: ByteRegister, destination: ByteRegister)(implicit processorMode: ProcessorMode): X86Operation =
      R8ToRM8(source, destination, opcodeBase, mnemonic)

    def apply[Size<:ExtendedSize](source: GeneralPurposeRegister with Size, destination: ModRMEncodableOperand with Size)(implicit processorMode: ProcessorMode): X86Operation =
      R16ToRM16(source, destination, opcodeBase, mnemonic)

    def apply[Size<:ExtendedSize](source: GeneralPurposeRegister with Size, destination: GeneralPurposeRegister with Size)(implicit processorMode: ProcessorMode): X86Operation =
      R16ToRM16(destination, source, opcodeBase, mnemonic)

    def apply(source: ModRMEncodableOperand with ByteSize, destination: ByteRegister)(implicit processorMode: ProcessorMode): X86Operation =
      RM8ToR8(destination, source, opcodeBase, mnemonic)

    def apply[Size<:ExtendedSize](source: ModRMEncodableOperand with Size, destination: GeneralPurposeRegister with Size)(implicit processorMode: ProcessorMode): X86Operation =
      RM16ToR16(destination, source, opcodeBase, mnemonic)

  }

  sealed class X64BasicInteraction(opcodeBase: Byte, extensionCode: Byte, mnemonic: String) {

    def apply(immediate: ImmediateValue with ByteSize, destination: Accumulator.LowByte.type)(implicit processorMode: ProcessorMode): X86Operation =
      Imm8ToAL(immediate, opcodeBase, mnemonic)

    def apply(immediate: ImmediateValue with WordSize, destination: Accumulator.Word.type)(implicit processorMode: ProcessorMode): X86Operation =
      Imm16ToAX(immediate, opcodeBase, mnemonic)

    def apply(immediate: ImmediateValue with DoubleWordSize, destination: Accumulator.DoubleWord.type)(implicit processorMode: ProcessorMode): X86Operation =
      Imm32ToEAX(immediate, opcodeBase, mnemonic)

    def apply(immediate: ImmediateValue with DoubleWordSize, destination: Accumulator.QuadWord.type)(implicit processorMode: ProcessorMode): X86Operation =
      Imm32ToRAX(immediate, opcodeBase, mnemonic)

    def apply[ImmediateSize<:ValueSize, DestinationSize<:ValueSize](immediate: ImmediateValue with ImmediateSize, destination: ModRMEncodableOperand with DestinationSize)(implicit processorMode: ProcessorMode): X86Operation =
      (immediate, destination) match {
        case (imm: ImmediateValue with ByteSize, d: ModRMEncodableOperand with WideSize) =>
          Imm8ToRM16(d, imm, extensionCode, opcodeBase, mnemonic)
        case (imm: ImmediateValue with ByteSize, d: ModRMEncodableOperand with ByteSize) =>
          Imm8ToRM8(d, imm, extensionCode, opcodeBase, mnemonic)
        case (imm: ImmediateValue with DoubleWordSize, d: ModRMEncodableOperand with QuadWordSize) =>
          Imm16ToRM16(d, imm, extensionCode, opcodeBase, mnemonic)
        case (_: ImmediateValue with QuadWordSize, _) =>
          throw new AssertionError
        case (_, dest: ValueSize) if !(dest sizeEquals immediate) =>
          throw new AssertionError
        case (imm: ImmediateValue with WideSize, d: ModRMEncodableOperand with WideSize) =>
          Imm16ToRM16(d, imm, extensionCode, opcodeBase, mnemonic)
        case _ =>
          throw new AssertionError
      }

    def apply(source: ByteRegister, destination: ModRMEncodableOperand with ByteSize)(implicit processorMode: ProcessorMode): X86Operation =
      R8ToRM8(source, destination, opcodeBase, mnemonic)

    def apply(source: ByteRegister, destination: ByteRegister)(implicit processorMode: ProcessorMode): X86Operation =
      R8ToRM8(source, destination, opcodeBase, mnemonic)

    def apply[Size<:WideSize](source: GeneralPurposeRegister with Size, destination: ModRMEncodableOperand with Size)(implicit processorMode: ProcessorMode): X86Operation =
      R16ToRM16(source, destination, opcodeBase, mnemonic)

    def apply[Size<:WideSize](source: GeneralPurposeRegister with Size, destination: GeneralPurposeRegister with Size)(implicit processorMode: ProcessorMode): X86Operation =
      R16ToRM16(destination, source, opcodeBase, mnemonic)

    def apply(source: ModRMEncodableOperand with ByteSize, destination: ByteRegister)(implicit processorMode: ProcessorMode): X86Operation =
      RM8ToR8(destination, source, opcodeBase, mnemonic)

    def apply[Size<:WideSize](source: ModRMEncodableOperand with Size, destination: GeneralPurposeRegister with Size)(implicit processorMode: ProcessorMode): X86Operation =
      RM16ToR16(destination, source, opcodeBase, mnemonic)

  }

  trait LegacyOperations {
    object Add extends I8086BasicInteraction(0x00.toByte, 0x00.toByte, "add")
    object AddCarry extends I8086BasicInteraction(0x10.toByte, 0x02.toByte, "adc")
    object And extends I8086BasicInteraction(0x20.toByte, 0x04.toByte, "and")
    object Compare extends I8086BasicInteraction(0x38.toByte, 0x07.toByte, "cmp")
    object Or extends I8086BasicInteraction(0x08.toByte, 0x01.toByte, "or")
    object Subtract extends I8086BasicInteraction(0x28.toByte, 0x05.toByte, "sub")
    object SubtractCarry extends I8086BasicInteraction(0x18.toByte, 0x03.toByte, "sbc")
    object Xor extends I8086BasicInteraction(0x30.toByte, 0x06.toByte, "xor")
    object Not {
    implicit val mnemonic: String = "not"

      def apply(operand: ModRMEncodableOperand with ValueSize)(implicit processorMode: ProcessorMode): X86Operation =
        operand match {
          case o:ByteSize => RM8(o, mnemonic)
          case o:WideSize => RM16(o, mnemonic)
        }
    }
  }

  trait I386Operations {
    object Add extends I386BasicInteraction(0x00.toByte, 0x00.toByte, "add")
    object AddCarry extends I386BasicInteraction(0x10.toByte, 0x02.toByte, "adc")
    object And extends I386BasicInteraction(0x20.toByte, 0x04.toByte, "and")
    object Compare extends I386BasicInteraction(0x38.toByte, 0x07.toByte, "cmp")
    object Or extends I386BasicInteraction(0x08.toByte, 0x01.toByte, "or")
    object Subtract extends I386BasicInteraction(0x28.toByte, 0x05.toByte, "sub")
    object SubtractCarry extends I386BasicInteraction(0x18.toByte, 0x03.toByte, "sbc")
    object Xor extends I386BasicInteraction(0x30.toByte, 0x06.toByte, "xor")
    object Not {
    implicit val mnemonic: String = "not"

      def apply(operand: ModRMEncodableOperand with ValueSize)(implicit processorMode: ProcessorMode): X86Operation =
        operand match {
          case o:ByteSize => RM8(o, mnemonic)
          case o:WideSize => RM16(o, mnemonic)
        }
    }
  }

  trait RealOperations extends I386Operations
  trait ProtectedOperations extends I386Operations

  trait LongOperations {
    object Add extends X64BasicInteraction(0x00.toByte, 0x00.toByte, "add")
    object AddCarry extends X64BasicInteraction(0x10.toByte, 0x02.toByte, "adc")
    object And extends X64BasicInteraction(0x20.toByte, 0x04.toByte, "and")
    object Compare extends X64BasicInteraction(0x38.toByte, 0x07.toByte, "cmp")
    object Or extends X64BasicInteraction(0x08.toByte, 0x01.toByte, "or")
    object Subtract extends X64BasicInteraction(0x28.toByte, 0x05.toByte, "sub")
    object SubtractCarry extends X64BasicInteraction(0x18.toByte, 0x03.toByte, "sbc")
    object Xor extends X64BasicInteraction(0x30.toByte, 0x06.toByte, "xor")
    object Not {
    implicit val mnemonic: String = "not"

      def apply(operand: ModRMEncodableOperand with ValueSize)(implicit processorMode: ProcessorMode): X86Operation =
        operand match {
          case o:ByteSize => RM8(o, mnemonic)
          case o:WideSize => RM16(o, mnemonic)
        }
    }
  }
}
