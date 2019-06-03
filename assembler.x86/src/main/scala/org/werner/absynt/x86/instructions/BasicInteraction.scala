package org.werner.absynt.x86.instructions

import org.werner.absynt.x86.HasOperandSizePrefixRequirements
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder._
import org.werner.absynt.x86.operations._


object BasicInteraction {

  trait Common {
    self: HasOperandSizePrefixRequirements =>
    protected def Imm8ToAL(immediateValue: ImmediateValue with ByteSize, opcodeBase: Byte, mnemonic: String): X86Operation =
      new Static((opcodeBase + 0x04).toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] with HasOperandSizePrefixRequirements {

        override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Accumulator.LowByte, destination)

        override val immediateOrder: OperandOrder = source
        override val immediate: ImmediateValue with ByteSize = immediateValue
      }

    protected def Imm16ToAX(immediateValue: ImmediateValue with WordSize, opcodeBase: Byte, mnemonic: String): X86Operation =
      new Static((opcodeBase + 0x05).toByte :: Nil, mnemonic) with NoDisplacement with Immediate[WordSize] with HasOperandSizePrefixRequirements {

        override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Accumulator.Word, destination)

        override val immediateOrder: OperandOrder = source
        override val immediate: ImmediateValue with WordSize = immediateValue
      }

    protected def Imm32ToEAX(immediateValue: ImmediateValue with DoubleWordSize, opcodeBase: Byte, mnemonic: String): X86Operation =
      new Static((opcodeBase + 0x5).toByte :: Nil, mnemonic) with NoDisplacement with Immediate[DoubleWordSize] with HasOperandSizePrefixRequirements {

        override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Accumulator.DoubleWord, destination)

        override val immediateOrder: OperandOrder = source
        override val immediate: ImmediateValue with DoubleWordSize = immediateValue
      }

    protected def Imm32ToRAX(immediateValue: ImmediateValue with DoubleWordSize, opcodeBase: Byte, mnemonic: String): X86Operation =
      new Static((opcodeBase + 0x5).toByte :: Nil, mnemonic) with NoDisplacement with Immediate[DoubleWordSize] with HasOperandSizePrefixRequirements {

        override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Accumulator.QuadWord, destination)

        override val immediateOrder: OperandOrder = source
        override val immediate: ImmediateValue with DoubleWordSize = immediateValue
      }

    protected def Imm8ToRM8(operand: ModRMEncodableOperand with ByteSize, immediateValue: ImmediateValue with ByteSize, extensionCode: Byte, opcodeBase: Byte, mnemonic: String): X86Operation =
      new ModRM(operand, 0x80.toByte :: Nil, extensionCode, mnemonic, destination) with NoDisplacement with Immediate[ByteSize] with HasOperandSizePrefixRequirements {

        override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        override val immediateOrder: OperandOrder = source
        override val immediate: ImmediateValue with ByteSize = immediateValue
      }

    protected def Imm16ToRM16[Size <: WordDoubleQuadSize](operand: ModRMEncodableOperand with Size, immediateValue: ImmediateValue with Size, extensionCode: Byte, opcodeBase: Byte, mnemonic: String): X86Operation =
      new ModRM(operand, 0x81.toByte :: Nil, extensionCode, mnemonic, destination) with NoDisplacement with Immediate[Size] with HasOperandSizePrefixRequirements {

        override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        override val immediateOrder: OperandOrder = source
        override val immediate: ImmediateValue with Size = immediateValue
      }

    protected def Imm8ToRM16[Size <: WordDoubleQuadSize](operand: ModRMEncodableOperand with Size, immediateValue: ImmediateValue with ByteSize, extensionCode: Byte, opcodeBase: Byte, mnemonic: String): X86Operation =
      new ModRM(operand, 0x83.toByte :: Nil, extensionCode, mnemonic, destination) with NoDisplacement with Immediate[ByteSize] with HasOperandSizePrefixRequirements {

        override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        override val immediateOrder: OperandOrder = source
        override val immediate: ImmediateValue with ByteSize = immediateValue
      }

    protected def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand with ByteSize, opcodeBase: Byte, mnemonic: String): X86Operation =
      new ModRRM(operand1, operand2, (opcodeBase + 0x00).toByte :: Nil, mnemonic, destination)

    protected def R16ToRM16[Size <: WordDoubleQuadSize](operand1: GeneralPurposeRegister with Size, operand2: ModRMEncodableOperand with Size, opcodeBase: Byte, mnemonic: String): X86Operation =
      new ModRRM(operand1, operand2, (opcodeBase + 0x01).toByte :: Nil, mnemonic, destination)

    protected def RM8ToR8(operand1: ByteRegister, operand2: ModRMEncodableOperand with ByteSize, opcodeBase: Byte, mnemonic: String): X86Operation =
      new ModRRM(operand1, operand2, (opcodeBase + 0x02).toByte :: Nil, mnemonic, source)

    protected def RM16ToR16[Size <: WordDoubleQuadSize](operand1: GeneralPurposeRegister with Size, operand2: ModRMEncodableOperand with Size, opcodeBase: Byte, mnemonic: String) =
      new ModRRM(operand1, operand2, (opcodeBase + 0x03).toByte :: Nil, mnemonic, source)

    protected def RM8(operand: ModRMEncodableOperand with ByteSize, mnemonic: String): X86Operation =
      new ModRM(operand, 0xF6.toByte :: Nil, 2, mnemonic, destination) with NoDisplacement with NoImmediate

    protected def RM16[Size <: WordDoubleQuadSize](operand: ModRMEncodableOperand with Size, mnemonic: String): X86Operation =
      new ModRM(operand, 0xF7.toByte :: Nil, 2, mnemonic, destination) with NoDisplacement with NoImmediate

  }


  trait LegacyOperations extends Common {
    self: HasOperandSizePrefixRequirements =>

    sealed class I8086BasicInteraction(opcodeBase: Byte, extensionCode: Byte, mnemonic: String) extends HasOperandSizePrefixRequirements {

      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = LegacyOperations.this.operandSizePrefixRequirement

      def apply(immediate: ImmediateValue with ByteSize, destination: Accumulator.LowByte.type): X86Operation =
        Imm8ToAL(immediate, opcodeBase, mnemonic)

      def apply(immediate: ImmediateValue with WordSize, destination: Accumulator.Word.type): X86Operation =
        Imm16ToAX(immediate, opcodeBase, mnemonic)

      def apply[ImmediateSize <: ValueSize, DestinationSize <: ValueSize](immediate: ImmediateValue with ImmediateSize, destination: ModRMEncodableOperand with DestinationSize): X86Operation =
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

      def apply(source: ByteRegister, destination: ModRMEncodableOperand with ByteSize): X86Operation =
        R8ToRM8(source, destination, opcodeBase, mnemonic)

      def apply(source: ByteRegister, destination: ByteRegister): X86Operation =
        R8ToRM8(source, destination, opcodeBase, mnemonic)

      def apply(source: GeneralPurposeRegister with WordSize, destination: ModRMEncodableOperand with WordSize): X86Operation =
        R16ToRM16(source, destination, opcodeBase, mnemonic)

      def apply(source: GeneralPurposeRegister with WordSize, destination: GeneralPurposeRegister with WordSize): X86Operation =
        R16ToRM16(destination, source, opcodeBase, mnemonic)

      def apply(source: ModRMEncodableOperand with ByteSize, destination: ByteRegister): X86Operation =
        RM8ToR8(destination, source, opcodeBase, mnemonic)

      def apply(source: ModRMEncodableOperand with WordSize, destination: GeneralPurposeRegister with WordSize): X86Operation =
        RM16ToR16(destination, source, opcodeBase, mnemonic)

    }

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

      def apply(operand: ModRMEncodableOperand with ValueSize): X86Operation =
        operand match {
          case o: ByteSize => RM8(o, mnemonic)
          case o: WordDoubleQuadSize => RM16(o, mnemonic)
        }
    }

  }

  trait I386Operations extends Common {
    self: HasOperandSizePrefixRequirements =>

    sealed class I386BasicInteraction(opcodeBase: Byte, extensionCode: Byte, mnemonic: String) extends HasOperandSizePrefixRequirements {

      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = I386Operations.this.operandSizePrefixRequirement

      def apply(immediate: ImmediateValue with ByteSize, destination: Accumulator.LowByte.type): X86Operation =
        Imm8ToAL(immediate, opcodeBase, mnemonic)

      def apply(immediate: ImmediateValue with WordSize, destination: Accumulator.Word.type): X86Operation =
        Imm16ToAX(immediate, opcodeBase, mnemonic)

      def apply(immediate: ImmediateValue with DoubleWordSize, destination: Accumulator.DoubleWord.type): X86Operation =
        Imm32ToEAX(immediate, opcodeBase, mnemonic)

      def apply[ImmediateSize <: ByteWordDoubleSize, DestinationSize <: ByteWordDoubleSize](immediate: ImmediateValue with ImmediateSize, destination: ModRMEncodableOperand with DestinationSize): X86Operation =
        (immediate, destination) match {
          case (imm: ImmediateValue with ByteSize, d: ModRMEncodableOperand with WordDoubleQuadSize) =>
            Imm8ToRM16(d, imm, extensionCode, opcodeBase, mnemonic)
          case (imm: ImmediateValue with ByteSize, d: ModRMEncodableOperand with ByteSize) =>
            Imm8ToRM8(d, imm, extensionCode, opcodeBase, mnemonic)
          case (imm: ImmediateValue with WordDoubleQuadSize, d: ModRMEncodableOperand with WordDoubleQuadSize)
            if d sizeEquals imm =>
            Imm16ToRM16(d, imm, extensionCode, opcodeBase, mnemonic)
          case _ =>
            throw new AssertionError
        }

      def apply(source: ByteRegister, destination: ModRMEncodableOperand with ByteSize): X86Operation =
        R8ToRM8(source, destination, opcodeBase, mnemonic)

      def apply(source: ByteRegister, destination: ByteRegister): X86Operation =
        R8ToRM8(source, destination, opcodeBase, mnemonic)

      def apply[Size <: WordDoubleSize](source: GeneralPurposeRegister with Size, destination: ModRMEncodableOperand with Size): X86Operation =
        R16ToRM16(source, destination, opcodeBase, mnemonic)

      def apply[Size <: WordDoubleSize](source: GeneralPurposeRegister with Size, destination: GeneralPurposeRegister with Size): X86Operation =
        R16ToRM16(destination, source, opcodeBase, mnemonic)

      def apply(source: ModRMEncodableOperand with ByteSize, destination: ByteRegister): X86Operation =
        RM8ToR8(destination, source, opcodeBase, mnemonic)

      def apply[Size <: WordDoubleSize](source: ModRMEncodableOperand with Size, destination: GeneralPurposeRegister with Size): X86Operation =
        RM16ToR16(destination, source, opcodeBase, mnemonic)

    }


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

      def apply(operand: ModRMEncodableOperand with ValueSize): X86Operation =
        operand match {
          case o: ByteSize => RM8(o, mnemonic)
          case o: WordDoubleQuadSize => RM16(o, mnemonic)
        }
    }

  }

  trait RealOperations extends I386Operations {
    self: HasOperandSizePrefixRequirements =>
  }

  trait ProtectedOperations extends I386Operations {
    self: HasOperandSizePrefixRequirements =>
  }

  trait LongOperations extends Common {
    self: HasOperandSizePrefixRequirements =>

    sealed class X64BasicInteraction(opcodeBase: Byte, extensionCode: Byte, mnemonic: String) extends HasOperandSizePrefixRequirements {

      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = LongOperations.this.operandSizePrefixRequirement

      def apply(immediate: ImmediateValue with ByteSize, destination: Accumulator.LowByte.type): X86Operation =
        Imm8ToAL(immediate, opcodeBase, mnemonic)

      def apply(immediate: ImmediateValue with WordSize, destination: Accumulator.Word.type): X86Operation =
        Imm16ToAX(immediate, opcodeBase, mnemonic)

      def apply(immediate: ImmediateValue with DoubleWordSize, destination: Accumulator.DoubleWord.type): X86Operation =
        Imm32ToEAX(immediate, opcodeBase, mnemonic)

      def apply(immediate: ImmediateValue with DoubleWordSize, destination: Accumulator.QuadWord.type): X86Operation =
        Imm32ToRAX(immediate, opcodeBase, mnemonic)

      def apply[ImmediateSize <: ByteWordDoubleSize, DestinationSize <: ValueSize](immediate: ImmediateValue with ImmediateSize, destination: ModRMEncodableOperand with DestinationSize): X86Operation =
        (immediate, destination) match {
          case (imm: ImmediateValue with ByteSize, d: ModRMEncodableOperand with WordDoubleQuadSize) =>
            Imm8ToRM16(d, imm, extensionCode, opcodeBase, mnemonic)
          case (imm: ImmediateValue with ByteSize, d: ModRMEncodableOperand with ByteSize) =>
            Imm8ToRM8(d, imm, extensionCode, opcodeBase, mnemonic)
          case (imm: ImmediateValue with DoubleWordSize, d: ModRMEncodableOperand with QuadWordSize) =>
            Imm16ToRM16(d, imm, extensionCode, opcodeBase, mnemonic)
          case (imm: ImmediateValue with WordDoubleSize, d: ModRMEncodableOperand with WordDoubleSize)
            if d sizeEquals imm =>
            Imm16ToRM16(d, imm, extensionCode, opcodeBase, mnemonic)
          case _ =>
            throw new AssertionError
        }

      def apply(source: ByteRegister, destination: ModRMEncodableOperand with ByteSize): X86Operation =
        R8ToRM8(source, destination, opcodeBase, mnemonic)

      def apply(source: ByteRegister, destination: ByteRegister): X86Operation =
        R8ToRM8(source, destination, opcodeBase, mnemonic)

      def apply[Size <: WordDoubleQuadSize](source: GeneralPurposeRegister with Size, destination: ModRMEncodableOperand with Size): X86Operation =
        R16ToRM16(source, destination, opcodeBase, mnemonic)

      def apply[Size <: WordDoubleQuadSize](source: GeneralPurposeRegister with Size, destination: GeneralPurposeRegister with Size): X86Operation =
        R16ToRM16(destination, source, opcodeBase, mnemonic)

      def apply(source: ModRMEncodableOperand with ByteSize, destination: ByteRegister): X86Operation =
        RM8ToR8(destination, source, opcodeBase, mnemonic)

      def apply[Size <: WordDoubleQuadSize](source: ModRMEncodableOperand with Size, destination: GeneralPurposeRegister with Size): X86Operation =
        RM16ToR16(destination, source, opcodeBase, mnemonic)
    }


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

      def apply(operand: ModRMEncodableOperand with ValueSize): X86Operation =
        operand match {
          case o: ByteSize => RM8(o, mnemonic)
          case o: WordDoubleQuadSize => RM16(o, mnemonic)
        }
    }

  }

}
