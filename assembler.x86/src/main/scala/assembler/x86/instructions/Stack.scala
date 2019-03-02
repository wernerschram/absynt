package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operations.OperandInfo.OperandOrder._
import assembler.x86.operations._

object Stack {
  private val pushOpcode: String = "push"


  private def R16[Size<:WideSize](register: GeneralPurposeRegister with Size)(implicit processorMode: ProcessorMode) =
    new RegisterEncoded[WideSize](register, Seq(0x50.toByte), pushOpcode) with NoDisplacement with NoImmediate {
      override def registerOrder: OperandOrder = destination
    }

  private def RM16(operand: ModRMEncodableOperand with WideSize)(implicit processorMode: ProcessorMode) =
    new ModRM(operand, 0xFF.toByte :: Nil, 0x06.toByte, pushOpcode, destination) with NoDisplacement with NoImmediate


  private def Imm8(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0x6A.toByte :: Nil, pushOpcode) with NoDisplacement with Immediate[ByteSize] {
      override def immediate: ImmediateValue with ByteSize = immediateValue

      override def immediateOrder: OperandOrder = destination
    }

  private def Imm16[Size<:ExtendedSize](immediateValue: ImmediateValue with Size)(implicit processorMode: ProcessorMode) =
    new Static(0x68.toByte :: Nil, pushOpcode) with NoDisplacement with Immediate[Size] {
      override def immediate: ImmediateValue with Size = immediateValue

      override def immediateOrder: OperandOrder = destination
    }

  private def StaticCS()(implicit processorMode: ProcessorMode) = new Static(0x0E.toByte :: Nil, pushOpcode) with NoDisplacement with NoImmediate
  private def StaticSS()(implicit processorMode: ProcessorMode) = new Static(0x16.toByte :: Nil, pushOpcode) with NoDisplacement with NoImmediate
  private def StaticDS()(implicit processorMode: ProcessorMode) = new Static(0x1E.toByte :: Nil, pushOpcode) with NoDisplacement with NoImmediate
  private def StaticES()(implicit processorMode: ProcessorMode) = new Static(0x06.toByte :: Nil, pushOpcode) with NoDisplacement with NoImmediate
  private def StaticFS()(implicit processorMode: ProcessorMode) = new Static(0x0F.toByte :: 0xA0.toByte :: Nil, pushOpcode) with NoDisplacement with NoImmediate
  private def StaticGS()(implicit processorMode: ProcessorMode) = new Static(0x0F.toByte :: 0xA8.toByte :: Nil, pushOpcode) with NoDisplacement with NoImmediate

  trait LegacyOperations {

    object Push {
      def apply(register: GeneralPurposeRegister with WordSize): X86Operation =
        R16(register)(ProcessorMode.Legacy)

      def apply(operand: ModRMEncodableOperand with WordSize): X86Operation =
        RM16(operand)(ProcessorMode.Legacy)

      def apply[Size <: LegacySize](immediate: ImmediateValue with Size)(implicit processorMode: ProcessorMode): Static =
        immediate match {
          case i: ImmediateValue with ByteSize => Imm8(i)(ProcessorMode.Legacy)
          case i: ImmediateValue with WordSize => Imm16(i)(ProcessorMode.Legacy)
        }

      def apply(segment: SegmentRegister): Static = segment match {
        case Segment.Code => StaticCS()(ProcessorMode.Legacy)
        case Segment.Stack => StaticSS()(ProcessorMode.Legacy)
        case Segment.Data => StaticDS()(ProcessorMode.Legacy)
        case Segment.Extra => StaticES()(ProcessorMode.Legacy)
        case Segment.MoreExtra => StaticFS()(ProcessorMode.Legacy)
        case Segment.StillMoreExtra => StaticGS()(ProcessorMode.Legacy)
      }
    }


    object PushAll {
      implicit val opcode: String = "pusha"

      def apply(): Static =
        new Static(0x60.toByte :: Nil, opcode)(ProcessorMode.Legacy) with NoDisplacement with NoImmediate
    }

    object PushFlags {
      implicit val opcode: String = "pushf"

      def apply(): Static = new Static(0x9C.toByte :: Nil, opcode)(ProcessorMode.Legacy) with NoDisplacement with NoImmediate
    }

  }

  trait RealOperations {

    object Push {
      def apply[Size <: ExtendedSize](register: GeneralPurposeRegister with Size): X86Operation =
          R16(register)(ProcessorMode.Real)

      def apply[Size <: ExtendedSize](operand: ModRMEncodableOperand with Size): X86Operation =
          RM16(operand)(ProcessorMode.Real)

      def apply[Size <: DisplacementSize](immediate: ImmediateValue with Size)(implicit processorMode: ProcessorMode): Static =
        immediate match {
          case i: ImmediateValue with ByteSize => Imm8(i)(ProcessorMode.Real)
          case i: ImmediateValue with ExtendedSize => Imm16(i)(ProcessorMode.Real)
        }

      def apply(segment: SegmentRegister): Static = segment match {
        case Segment.Code => StaticCS()(ProcessorMode.Real)
        case Segment.Stack => StaticSS()(ProcessorMode.Real)
        case Segment.Data => StaticDS()(ProcessorMode.Real)
        case Segment.Extra => StaticES()(ProcessorMode.Real)
        case Segment.MoreExtra => StaticFS()(ProcessorMode.Real)
        case Segment.StillMoreExtra => StaticGS()(ProcessorMode.Real)
      }
    }

    object PushAll {
      implicit val opcode: String = "pusha"

      def apply(): Static =
        new Static(0x60.toByte :: Nil, opcode)(ProcessorMode.Real) with NoDisplacement with NoImmediate
    }

    object PushFlags {
      implicit val opcode: String = "pushf"

      def apply(): Static =
        new Static(0x9C.toByte :: Nil, opcode)(ProcessorMode.Real) with NoDisplacement with NoImmediate
    }
  }

  trait ProtectedOperations {

    object Push {
      def apply[Size <: ExtendedSize](register: GeneralPurposeRegister with Size): X86Operation =
          R16(register)(ProcessorMode.Protected)

      def apply[Size <: ExtendedSize](operand: ModRMEncodableOperand with Size): X86Operation =
          RM16(operand)(ProcessorMode.Protected)

      def apply[Size <: DisplacementSize](immediate: ImmediateValue with Size)(implicit processorMode: ProcessorMode): Static =
        immediate match {
          case i: ImmediateValue with ByteSize => Imm8(i)(ProcessorMode.Protected)
          case i: ImmediateValue with ExtendedSize => Imm16(i)(ProcessorMode.Protected)
        }

      def apply(segment: SegmentRegister): Static = segment match {
        case Segment.Code => StaticCS()(ProcessorMode.Protected)
        case Segment.Stack => StaticSS()(ProcessorMode.Protected)
        case Segment.Data => StaticDS()(ProcessorMode.Protected)
        case Segment.Extra => StaticES()(ProcessorMode.Protected)
        case Segment.MoreExtra => StaticFS()(ProcessorMode.Protected)
        case Segment.StillMoreExtra => StaticGS()(ProcessorMode.Protected)
      }
    }


    object PushAll {
      implicit val opcode: String = "pusha"

      def apply(): Static =
        new Static(0x60.toByte :: Nil, opcode)(ProcessorMode.Protected) with NoDisplacement with NoImmediate
    }

    object PushFlags {
      implicit val opcode: String = "pushf"

      def apply(): Static =
        new Static(0x9C.toByte :: Nil, opcode)(ProcessorMode.Protected) with NoDisplacement with NoImmediate
    }
  }

  trait LongOperations {

    object Push {
      def apply[Size <: WideSize](register: GeneralPurposeRegister with Size): X86Operation =
        register match {
          case r: GeneralPurposeRegister with WordSize =>
            R16(r)(ProcessorMode.Long)
          case r: GeneralPurposeRegister with QuadWordSize =>
            R16(r)(ProcessorMode.Long)
          case _ =>
            throw new AssertionError
        }

      def apply[Size <: WideSize](operand: ModRMEncodableOperand with Size): X86Operation =
        operand match {
          case o: WordSize =>
            RM16(o)(ProcessorMode.Long)
          case o: QuadWordSize =>
            RM16(o)(ProcessorMode.Long)
          case _ =>
            throw new AssertionError
        }

      def apply(immediate: ImmediateValue with DisplacementSize): Static =
        immediate match {
          case i: ImmediateValue with ByteSize => Imm8(i)(ProcessorMode.Long)
          case i: ImmediateValue with ExtendedSize => Imm16(i)(ProcessorMode.Long)
        }

      def apply(segment: SegmentRegister): Static = segment match {
        case Segment.Code => StaticCS()(ProcessorMode.Long)
        case Segment.Stack => StaticSS()(ProcessorMode.Long)
        case Segment.Data => StaticDS()(ProcessorMode.Long)
        case Segment.Extra => StaticES()(ProcessorMode.Long)
        case Segment.MoreExtra => StaticFS()(ProcessorMode.Long)
        case Segment.StillMoreExtra => StaticGS()(ProcessorMode.Long)
      }
    }

    object PushFlags {
      implicit val opcode: String = "pushf"

      def apply(): Static =
        new Static(0x9C.toByte :: Nil, opcode)(ProcessorMode.Long) with NoDisplacement with NoImmediate
    }
  }
}


