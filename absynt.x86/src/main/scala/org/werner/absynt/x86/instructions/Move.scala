package org.werner.absynt.x86.instructions

import org.werner.absynt._
import org.werner.absynt.resource.{AbsoluteReference, UnlabeledEncodable}
import org.werner.absynt.x86.HasOperandSizePrefixRequirements
import org.werner.absynt.x86.operands.ImmediateValue.{ValueToDoubleWordImmediate, ValueToQuadWordImmediate, ValueToWordImmediate}
import org.werner.absynt.x86.operands.Register.I8086GenericRegisters
import org.werner.absynt.x86.operands.memoryaccess._
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder._
import org.werner.absynt.x86.operations.{Immediate, ModRM, ModRRM, ModSegmentRM, NoDisplacement, NoImmediate, OperandInfo, OperandSizePrefixRequirement, RegisterEncoded, Static, X86Operation, MemoryLocation => MemoryLocationOperation}

object Move extends I8086GenericRegisters {

  implicit val mnemonic: String = "mov"

  sealed trait Common {
    self: HasOperandSizePrefixRequirements =>

    protected def RM16ToSReg[Size <: WordDoubleQuadSize](operand1: SegmentRegister, operand2: ModRMEncodableOperand with Size) =
      new ModSegmentRM(operand1, operand2, 0x8E.toByte :: Nil, mnemonic, source) with NoDisplacement with NoImmediate

    protected def SRegToRM16[Size <: WordDoubleQuadSize](operand1: SegmentRegister, operand2: ModRMEncodableOperand with Size) =
      new ModSegmentRM(operand1, operand2, 0x8C.toByte :: Nil, mnemonic, destination) with NoDisplacement with NoImmediate

    protected def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand with ByteSize) =
      new ModRRM(operand1, operand2, 0x88.toByte :: Nil, mnemonic, destination)

    protected def ALToMOffs8(memoryLocation: MemoryLocation with ByteSize): X86Operation =
      new Static(0xA2.toByte :: Nil, mnemonic) with MemoryLocationOperation[ByteSize] with NoImmediate with HasOperandSizePrefixRequirements {
        override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(AL, source)

        override val location: MemoryLocation with ByteSize = memoryLocation

        override def offsetOrder: OperandOrder = destination

      }

    protected def R16ToRM16[Size <: WordDoubleQuadSize](operand1: GeneralPurposeRegister with Size, operand2: ModRMEncodableOperand with Size) =
      new ModRRM(operand1, operand2, 0x89.toByte :: Nil, mnemonic, destination)

    protected def AXToMOffs16[Size <: WordDoubleQuadSize](accumulatorRegister: AccumulatorRegister with Size, memoryLocation: MemoryLocation with Size): Static with MemoryLocationOperation[Size] with NoImmediate =
      new Static(0xA3.toByte :: Nil, mnemonic) with MemoryLocationOperation[Size] with NoImmediate with HasOperandSizePrefixRequirements {
        override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(accumulatorRegister, source)

        override val location: MemoryLocation with Size = memoryLocation

        override def offsetOrder: OperandOrder = destination
      }

    protected def RM8ToR8(operand1: ByteRegister, operand2: ModRMEncodableOperand with ByteSize) =
      new ModRRM(operand1, operand2, 0x8A.toByte :: Nil, mnemonic, source)

    protected def MOffs8ToAL(memoryLocation: MemoryLocation with ByteSize): Static with MemoryLocationOperation[ByteSize] with NoImmediate =
      new Static(0xA0.toByte :: Nil, mnemonic) with MemoryLocationOperation[ByteSize] with NoImmediate with HasOperandSizePrefixRequirements {
        override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(AL, destination)

        override val location: MemoryLocation with ByteSize = memoryLocation

        override def offsetOrder: OperandOrder = source
      }

    protected def RM16ToR16[Size <: WordDoubleQuadSize](operand1: GeneralPurposeRegister with Size, operand2: ModRMEncodableOperand with Size) =
      new ModRRM(operand1, operand2, 0x8B.toByte :: Nil, mnemonic, source)

    protected def MOffs16ToAX[Size <: WordDoubleQuadSize](memoryLocation: MemoryLocation with Size, accumulatorRegister: AccumulatorRegister with Size): Static with MemoryLocationOperation[Size] with NoImmediate =
      new Static(0xA1.toByte :: Nil, mnemonic) with MemoryLocationOperation[Size] with NoImmediate with HasOperandSizePrefixRequirements {
        override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(accumulatorRegister, destination)

        override val location: MemoryLocation with Size = memoryLocation

        override def offsetOrder: OperandOrder = source
      }

    protected def Imm8ToR8(register: ByteRegister, immediateValue: ImmediateValue with ByteSize): RegisterEncoded[ByteSize] with NoDisplacement with Immediate[ByteSize] =
      new RegisterEncoded[ByteSize](register, Seq(0xB0.toByte), mnemonic) with NoDisplacement with Immediate[ByteSize] with HasOperandSizePrefixRequirements {
        override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        override def immediate: ImmediateValue with ByteSize = immediateValue

        override def immediateOrder: OperandOrder = source

        override def registerOrder: OperandOrder = destination
      }

    protected def Imm16ToR16[Size <: WordDoubleQuadSize](register: GeneralPurposeRegister with Size, immediateValue: ImmediateValue with Size): RegisterEncoded[Size] with NoDisplacement with Immediate[Size] =
      new RegisterEncoded[Size](register, Seq(0xB8.toByte), mnemonic) with NoDisplacement with Immediate[Size] with HasOperandSizePrefixRequirements {
        override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        override def immediate: ImmediateValue with Size = immediateValue

        override def immediateOrder: OperandOrder = source

        override def registerOrder: OperandOrder = destination
      }

    protected def Imm8ToRM8(operand: ModRMEncodableOperand with ByteSize, immediateValue: ImmediateValue with ByteSize): ModRM[ModRMEncodableOperand with ByteSize] with NoDisplacement with Immediate[ByteSize] =
      new ModRM(operand, 0xC6.toByte :: Nil, 0, mnemonic, destination) with NoDisplacement with Immediate[ByteSize] with HasOperandSizePrefixRequirements {
        override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        override def immediate: ImmediateValue with ByteSize = immediateValue

        override def immediateOrder: OperandOrder = source
      }

    protected def Imm16ToRM16[OperandSize <: WordDoubleQuadSize](operand: ModRMEncodableOperand with OperandSize, immediateValue: ImmediateValue with OperandSize): ModRM[ModRMEncodableOperand with OperandSize] with NoDisplacement with Immediate[OperandSize] =
      new ModRM(operand, 0xC7.toByte :: Nil, 0, mnemonic, destination) with NoDisplacement with Immediate[OperandSize] with HasOperandSizePrefixRequirements {
        override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        override def immediate: ImmediateValue with OperandSize = immediateValue

        override def immediateOrder: OperandOrder = source
      }

    sealed abstract class MoveForLabel(targetLabel: Label) extends AbsoluteReference(targetLabel) {
      def size: Int

      override def sizeForDistance(distance: Int): Int = size

      override def possibleSizes: Set[Int] = Set(size)
    }

    def apply(source: Accumulator.LowByte.type, destination: MemoryAddress with ByteSize): X86Operation =
      ALToMOffs8(destination)

    def apply(source: ByteRegister, destination: ModRMEncodableOperand with ByteSize): X86Operation =
      R8ToRM8(source, destination)

    def apply(source: MemoryAddress with ByteSize, accumulator: Accumulator.LowByte.type): X86Operation =
      MOffs8ToAL(source)

    def apply(source: ModRMEncodableOperand with ByteSize, destination: ByteRegister): X86Operation =
      RM8ToR8(destination, source)
  }

  sealed trait I8086 extends Common {
    self: HasOperandSizePrefixRequirements =>
    def apply(source: ByteRegister, destination: ByteRegister): X86Operation =
      apply(source, destination.asInstanceOf[ModRMEncodableOperand with ByteSize])

    def apply(source: ModRMEncodableOperand with WordSize, destination: SegmentRegister): X86Operation =
      RM16ToSReg(destination, source)

    def apply(source: SegmentRegister, destination: ModRMEncodableOperand with WordSize): X86Operation =
      SRegToRM16(source, destination)

    def apply(accumulator: AccumulatorRegister with WordSize, destination: MemoryAddress with WordSize): X86Operation =
      AXToMOffs16(accumulator, destination)

    def apply(source: GeneralPurposeRegister with WordSize, destination: ModRMEncodableOperand with WordSize): X86Operation =
      R16ToRM16(source, destination)

    def apply(source: GeneralPurposeRegister with WordSize, destination: GeneralPurposeRegister with WordSize): X86Operation =
      apply(source, destination.asInstanceOf[ModRMEncodableOperand with WordSize])

    def apply(source: MemoryAddress with WordSize, accumulator: Accumulator.Word.type): X86Operation =
      MOffs16ToAX(source, accumulator)

    def apply(source: ModRMEncodableOperand with WordSize, destination: GeneralPurposeRegister with WordSize): X86Operation =
      RM16ToR16(destination, source)

    def apply(source: ImmediateValue with WordSize, destination: GeneralPurposeRegister with WordSize): X86Operation =
      Imm16ToR16(destination, source)

    def apply(source: ImmediateValue with ByteSize, destination: ByteRegister): X86Operation =
      Imm8ToR8(destination, source)

    def apply(source: ImmediateValue with WordSize, destination: ModRMEncodableOperand with WordSize): X86Operation =
      (source, destination) match {
        case (s: ImmediateValue with ByteSize, d: ModRMEncodableOperand with ByteSize) =>
          Imm8ToRM8(d, s)
        case (s: ImmediateValue with WordSize, d: ModRMEncodableOperand with WordSize) =>
          Imm16ToRM16(d, s)
      }
  }

  sealed trait I386 extends Common {
    self: HasOperandSizePrefixRequirements =>
    def apply(source: ByteRegister, destination: ByteRegister): X86Operation =
      apply(source, destination.asInstanceOf[ModRMEncodableOperand with ByteSize])

    def apply[Size <: WordDoubleSize](source: ModRMEncodableOperand with Size, destination: SegmentRegister): X86Operation =
      RM16ToSReg(destination, source)

    def apply[Size <: WordDoubleSize](source: SegmentRegister, destination: ModRMEncodableOperand with Size): X86Operation =
      SRegToRM16(source, destination)

    def apply[Size <: WordDoubleSize](accumulator: AccumulatorRegister with Size, destination: MemoryAddress with Size): X86Operation =
      AXToMOffs16(accumulator, destination)

    def apply[Size <: WordDoubleSize](source: GeneralPurposeRegister with Size, destination: ModRMEncodableOperand with Size): X86Operation =
      R16ToRM16(source, destination)

    def apply[Size <: WordDoubleSize](source: GeneralPurposeRegister with Size, destination: GeneralPurposeRegister with Size): X86Operation =
      apply(source, destination.asInstanceOf[ModRMEncodableOperand with Size])

    def apply[Size <: WordDoubleSize](source: MemoryAddress with Size, accumulator: AccumulatorRegister with Size): X86Operation =
      MOffs16ToAX(source, accumulator)

    def apply[Size <: WordDoubleSize](source: ModRMEncodableOperand with Size, destination: GeneralPurposeRegister with Size): X86Operation =
      RM16ToR16(destination, source)

    def apply[Size <: WordDoubleSize](source: ImmediateValue with Size, destination: GeneralPurposeRegister with Size): X86Operation =
      Imm16ToR16(destination, source)

    def apply(source: ImmediateValue with ByteSize, destination: ByteRegister): X86Operation =
      Imm8ToR8(destination, source)

    def apply[Size <: ByteWordDoubleSize](source: ImmediateValue with Size, destination: ModRMEncodableOperand with Size): X86Operation =
      (source, destination) match {
        case (s: ImmediateValue with ByteSize, d: ModRMEncodableOperand with ByteSize) =>
          Imm8ToRM8(d, s)
        case (s: ImmediateValue with WordSize, d: ModRMEncodableOperand with WordSize) =>
          Imm16ToRM16(d, s)
        case (s: ImmediateValue with DoubleWordSize, d: ModRMEncodableOperand with DoubleWordSize) =>
          Imm16ToRM16(d, s)
      }

  }

  trait LegacyOperations {
    self: HasOperandSizePrefixRequirements =>
    object Move extends I8086 with HasOperandSizePrefixRequirements {

      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = LegacyOperations.this.operandSizePrefixRequirement

      def forLabel(targetLabel: Label, register: GeneralPurposeRegister with WordSize)(implicit wordImmediate: ValueToWordImmediate): AbsoluteReference =
        new MoveForLabel(targetLabel) {
          override val size: Int = 3

          override def encodableForDistance(distance: Int): UnlabeledEncodable =
            Imm16ToR16(register, wordImmediate(distance.toShort))
        }
    }
  }

  trait RealOperations {
    self: HasOperandSizePrefixRequirements =>
    object Move extends I386 with HasOperandSizePrefixRequirements {

      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = RealOperations.this.operandSizePrefixRequirement

      def forLabel(targetLabel: Label, register: GeneralPurposeRegister with WordSize)(implicit wordImmediate: ValueToWordImmediate): AbsoluteReference =
        new MoveForLabel(targetLabel) {
          override val size: Int = 3

          override def encodableForDistance(distance: Int): UnlabeledEncodable =
            Imm16ToR16(register, wordImmediate(distance.toShort))
        }
    }
  }

  trait ProtectedOperations {
    self: HasOperandSizePrefixRequirements =>
    object Move extends I386 with HasOperandSizePrefixRequirements {

      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = ProtectedOperations.this.operandSizePrefixRequirement

      def forLabel(targetLabel: Label, register: GeneralPurposeRegister with DoubleWordSize)(implicit doubleWordImmediate: ValueToDoubleWordImmediate): AbsoluteReference =
        new MoveForLabel(targetLabel) {
          override val size: Int = 5

          override def encodableForDistance(distance: Int): UnlabeledEncodable =
            Imm16ToR16[DoubleWordSize](register, doubleWordImmediate(distance))
        }
    }
  }

  trait LongOperations {
    self: HasOperandSizePrefixRequirements =>
    object Move extends Common with HasOperandSizePrefixRequirements {

      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = LongOperations.this.operandSizePrefixRequirement

      def apply[Size <: WordDoubleQuadSize](source: ModRMEncodableOperand with Size, destination: SegmentRegister): X86Operation =
        RM16ToSReg(destination, source)

      def apply[Size <: WordDoubleQuadSize](source: SegmentRegister, destination: ModRMEncodableOperand with Size): X86Operation =
        SRegToRM16(source, destination)

      def apply(source: ByteRegister, destination: ByteRegister): X86Operation = {
        assume(!(source.isInstanceOf[GeneralPurposeRexRegister] && destination.isInstanceOf[HighByteRegister]))
        assume(!(source.isInstanceOf[HighByteRegister] && destination.isInstanceOf[GeneralPurposeRexRegister]))
        apply(source, destination.asInstanceOf[ModRMEncodableOperand with ByteSize])
      }

      def apply[Size <: WordDoubleQuadSize](accumulator: AccumulatorRegister with Size, destination: MemoryAddress with Size): X86Operation =
        AXToMOffs16(accumulator, destination)

      def apply[Size <: WordDoubleQuadSize](source: GeneralPurposeRegister with Size, destination: ModRMEncodableOperand with Size): X86Operation =
        R16ToRM16(source, destination)

      def apply[Size <: WordDoubleQuadSize](source: GeneralPurposeRegister with Size, destination: GeneralPurposeRegister with Size): X86Operation =
        apply(source, destination.asInstanceOf[ModRMEncodableOperand with Size])

      def apply[Size <: WordDoubleQuadSize](source: MemoryAddress with Size, accumulator: AccumulatorRegister with Size): X86Operation =
        MOffs16ToAX(source, accumulator)

      def apply[Size <: WordDoubleQuadSize](source: ModRMEncodableOperand with Size, destination: GeneralPurposeRegister with Size): X86Operation =
        RM16ToR16(destination, source)

      def apply[Size <: WordDoubleQuadSize](source: ImmediateValue with Size, destination: GeneralPurposeRegister with Size): X86Operation =
        Imm16ToR16(destination, source)

      def apply(source: ImmediateValue with ByteSize, destination: ByteRegister): X86Operation =
        Imm8ToR8(destination, source)

      def apply[Size <: ValueSize](source: ImmediateValue with Size, destination: ModRMEncodableOperand with Size): X86Operation =
        (source, destination) match {
          case (s: ImmediateValue with ByteSize, d: ModRMEncodableOperand with ByteSize) =>
            Imm8ToRM8(d, s)
          case (s: ImmediateValue with WordSize, d: ModRMEncodableOperand with WordSize) =>
            Imm16ToRM16(d, s)
          case (s: ImmediateValue with DoubleWordSize, d: ModRMEncodableOperand with DoubleWordSize) =>
            Imm16ToRM16(d, s)
          case (s: ImmediateValue with QuadWordSize, d: ModRMEncodableOperand with QuadWordSize) =>
            Imm16ToRM16(d, s)
        }

      def forLabel(targetLabel: Label, register: GeneralPurposeRegister with QuadWordSize)(implicit quadWordImmediate: ValueToQuadWordImmediate): AbsoluteReference =
        new MoveForLabel(targetLabel) {
          override val size: Int = 10

          override def encodableForDistance(distance: Int): UnlabeledEncodable =
            Imm16ToR16(register, quadWordImmediate(distance))
        }
    }
  }

}
