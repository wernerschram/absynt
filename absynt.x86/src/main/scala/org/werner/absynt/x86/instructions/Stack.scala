package org.werner.absynt.x86.instructions

import org.werner.absynt.x86.HasOperandSizePrefixRequirements
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder._
import org.werner.absynt.x86.operations._

object Stack {
  private val pushOpcode: String = "push"

  trait Common {
    self: HasOperandSizePrefixRequirements =>

    type RMMaxSize <: WordDoubleQuadSize
    type ImmMaxSize <: ValueSize
    type ImmExtendedMaxSize <: WordDoubleQuadSize

    protected def R16[Size <: RMMaxSize](register: GeneralPurposeRegister with Size): X86Operation =
      new RegisterEncoded[RMMaxSize](register, Seq(0x50.toByte), pushOpcode) with NoDisplacement with NoImmediate {
        override def registerOrder: OperandOrder = destination
      }

    protected def RM16(operand: ModRMEncodableOperand with RMMaxSize) =
      new ModRM(operand, 0xFF.toByte :: Nil, 0x06.toByte, pushOpcode, destination) with NoDisplacement with NoImmediate


    protected def Imm8(immediateValue: ImmediateValue with ByteSize): X86Operation =
      new Static(0x6A.toByte :: Nil, pushOpcode) with NoDisplacement with Immediate[ByteSize] with HasOperandSizePrefixRequirements {
        implicit override val operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        override def immediate: ImmediateValue with ByteSize = immediateValue

        override def immediateOrder: OperandOrder = destination
      }

    protected def Imm16[Size <: ImmExtendedMaxSize](immediateValue: ImmediateValue with Size): X86Operation =
      new Static(0x68.toByte :: Nil, pushOpcode) with NoDisplacement with Immediate[Size] with HasOperandSizePrefixRequirements {
        implicit override val operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        override def immediate: ImmediateValue with Size = immediateValue

        override def immediateOrder: OperandOrder = destination
      }

    protected def StaticCS() = new Static(0x0E.toByte :: Nil, pushOpcode) with NoDisplacement with NoImmediate
    protected def StaticSS() = new Static(0x16.toByte :: Nil, pushOpcode) with NoDisplacement with NoImmediate
    protected def StaticDS() = new Static(0x1E.toByte :: Nil, pushOpcode) with NoDisplacement with NoImmediate
    protected def StaticES() = new Static(0x06.toByte :: Nil, pushOpcode) with NoDisplacement with NoImmediate
    protected def StaticFS() = new Static(0x0F.toByte :: 0xA0.toByte :: Nil, pushOpcode) with NoDisplacement with NoImmediate
    protected def StaticGS() = new Static(0x0F.toByte :: 0xA8.toByte :: Nil, pushOpcode) with NoDisplacement with NoImmediate

    trait PushOperations {
      def apply(segment: SegmentRegister): Static = segment match {
        case Segment.Code => StaticCS()
        case Segment.Stack => StaticSS()
        case Segment.Data => StaticDS()
        case Segment.Extra => StaticES()
        case Segment.MoreExtra => StaticFS()
        case Segment.StillMoreExtra => StaticGS()
      }

      def apply[Size <: RMMaxSize](register: GeneralPurposeRegister with Size): X86Operation =
        R16(register)

      def apply[Size <: RMMaxSize](operand: ModRMEncodableOperand with Size): X86Operation =
        RM16(operand)

      def apply(immediate: ImmediateValue with ImmMaxSize): X86Operation =
        immediate match {
          case i: ByteSize => Imm8(i)
          // unchecked: type erasure does apply but every ImmMaxSize that is not ByteSize is ImmExtendedMaxSize
          case i: ImmExtendedMaxSize @unchecked => Imm16(i)
        }
    }
  }

  trait LegacyOperations extends Common {
    self: HasOperandSizePrefixRequirements =>

    override type RMMaxSize = WordSize
    override type ImmMaxSize = ByteWordSize
    override type ImmExtendedMaxSize = WordSize

    object Push extends PushOperations

    object PushAll {
      implicit val opcode: String = "pusha"

      def apply(): Static =
        new Static(0x60.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }

    object PushFlags {
      implicit val opcode: String = "pushf"

      def apply(): Static = new Static(0x9C.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }

  }

  trait RealOperations extends Common {
    self: HasOperandSizePrefixRequirements =>

    override type RMMaxSize = WordDoubleSize
    override type ImmMaxSize = ByteWordDoubleSize
    override type ImmExtendedMaxSize = WordDoubleSize

    object Push extends PushOperations

    object PushAll {
      implicit val opcode: String = "pusha"

      def apply(): Static =
        new Static(0x60.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }

    object PushFlags {
      implicit val opcode: String = "pushf"

      def apply(): Static =
        new Static(0x9C.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }
  }

  trait ProtectedOperations extends Common {
    self: HasOperandSizePrefixRequirements =>

    override type RMMaxSize = WordDoubleSize
    override type ImmMaxSize = ByteWordDoubleSize
    override type ImmExtendedMaxSize = WordDoubleSize

    object Push extends PushOperations

    object PushAll {
      implicit val opcode: String = "pusha"

      def apply(): Static =
        new Static(0x60.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }

    object PushFlags {
      implicit val opcode: String = "pushf"

      def apply(): Static =
        new Static(0x9C.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }
  }

  trait LongOperations extends Common {
    self: HasOperandSizePrefixRequirements =>

    override type RMMaxSize = WordQuadSize
    override type ImmMaxSize = ByteWordDoubleSize
    override type ImmExtendedMaxSize = WordDoubleSize

    object Push extends PushOperations

    object PushFlags {
      implicit val opcode: String = "pushf"

      def apply(): Static =
        new Static(0x9C.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }
  }
}


