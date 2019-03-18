package assembler.x86.instructions

import assembler.x86.HasOperandSizePrefixRequirements
import assembler.x86.operands._
import assembler.x86.operations.OperandInfo.OperandOrder._
import assembler.x86.operations._

object Stack {
  private val pushOpcode: String = "push"

  trait Common {
    self: HasOperandSizePrefixRequirements =>

    protected def R16[Size <: WordDoubleQuadSize](register: GeneralPurposeRegister with Size): X86Operation =
      new RegisterEncoded[WordDoubleQuadSize](register, Seq(0x50.toByte), pushOpcode) with NoDisplacement with NoImmediate {
        override def registerOrder: OperandOrder = destination
      }

    protected def RM16(operand: ModRMEncodableOperand with WordDoubleQuadSize) =
      new ModRM(operand, 0xFF.toByte :: Nil, 0x06.toByte, pushOpcode, destination) with NoDisplacement with NoImmediate


    protected def Imm8(immediateValue: ImmediateValue with ByteSize): X86Operation =
      new Static(0x6A.toByte :: Nil, pushOpcode) with NoDisplacement with Immediate[ByteSize] with HasOperandSizePrefixRequirements {
        implicit override val operandSizePrefixRequirement: OperandSizePrefixRequirement = Common.this.operandSizePrefixRequirement

        override def immediate: ImmediateValue with ByteSize = immediateValue

        override def immediateOrder: OperandOrder = destination
      }

    protected def Imm16[Size <: WordDoubleSize](immediateValue: ImmediateValue with Size): X86Operation =
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
  }

  trait LegacyOperations extends Common {
    self: HasOperandSizePrefixRequirements =>

    object Push {
      def apply(register: GeneralPurposeRegister with WordSize): X86Operation =
        R16(register)

      def apply(operand: ModRMEncodableOperand with WordSize): X86Operation =
        RM16(operand)

      def apply[Size <: ByteWordSize](immediate: ImmediateValue with Size): X86Operation =
        immediate match {
          case i: ImmediateValue with ByteSize => Imm8(i)
          case i: ImmediateValue with WordSize => Imm16(i)
        }

      def apply(segment: SegmentRegister): Static = segment match {
        case Segment.Code => StaticCS()
        case Segment.Stack => StaticSS()
        case Segment.Data => StaticDS()
        case Segment.Extra => StaticES()
        case Segment.MoreExtra => StaticFS()
        case Segment.StillMoreExtra => StaticGS()
      }
    }


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

    object Push {
      def apply[Size <: WordDoubleSize](register: GeneralPurposeRegister with Size): X86Operation =
          R16(register)

      def apply[Size <: WordDoubleSize](operand: ModRMEncodableOperand with Size): X86Operation =
          RM16(operand)

      def apply[Size <: ByteWordDoubleSize](immediate: ImmediateValue with Size): X86Operation =
        immediate match {
          case i: ImmediateValue with ByteSize => Imm8(i)
          case i: ImmediateValue with WordDoubleSize => Imm16(i)
        }

      def apply(segment: SegmentRegister): Static = segment match {
        case Segment.Code => StaticCS()
        case Segment.Stack => StaticSS()
        case Segment.Data => StaticDS()
        case Segment.Extra => StaticES()
        case Segment.MoreExtra => StaticFS()
        case Segment.StillMoreExtra => StaticGS()
      }
    }

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

    object Push {
      def apply[Size <: WordDoubleSize](register: GeneralPurposeRegister with Size): X86Operation = R16(register)

      def apply[Size <: WordDoubleSize](operand: ModRMEncodableOperand with Size): X86Operation =
          RM16(operand)

      def apply[Size <: ByteWordDoubleSize](immediate: ImmediateValue with Size): X86Operation =
        immediate match {
          case i: ImmediateValue with ByteSize => Imm8(i)
          case i: ImmediateValue with WordDoubleSize => Imm16(i)
        }

      def apply(segment: SegmentRegister): Static = segment match {
        case Segment.Code => StaticCS()
        case Segment.Stack => StaticSS()
        case Segment.Data => StaticDS()
        case Segment.Extra => StaticES()
        case Segment.MoreExtra => StaticFS()
        case Segment.StillMoreExtra => StaticGS()
      }
    }


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

    object Push {
      def apply[Size <: WordDoubleQuadSize](register: GeneralPurposeRegister with Size): X86Operation =
        register match {
          case r: GeneralPurposeRegister with WordSize =>
            R16(r)
          case r: GeneralPurposeRegister with QuadWordSize =>
            R16(r)
          case _ =>
            throw new AssertionError
        }

      def apply[Size <: WordDoubleQuadSize](operand: ModRMEncodableOperand with Size): X86Operation =
        operand match {
          case o: WordSize =>
            RM16(o)
          case o: QuadWordSize =>
            RM16(o)
          case _ =>
            throw new AssertionError
        }

      def apply(immediate: ImmediateValue with ByteWordDoubleSize): X86Operation =
        immediate match {
          case i: ImmediateValue with ByteSize => Imm8(i)
          case i: ImmediateValue with WordDoubleSize => Imm16(i)
        }

      def apply(segment: SegmentRegister): Static = segment match {
        case Segment.Code => StaticCS()
        case Segment.Stack => StaticSS()
        case Segment.Data => StaticDS()
        case Segment.Extra => StaticES()
        case Segment.MoreExtra => StaticFS()
        case Segment.StillMoreExtra => StaticGS()
      }
    }

    object PushFlags {
      implicit val opcode: String = "pushf"

      def apply(): Static =
        new Static(0x9C.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }
  }
}


