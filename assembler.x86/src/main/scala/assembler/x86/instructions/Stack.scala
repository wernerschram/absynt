package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operations.{Immediate, ModRMStatic, RegisterEncoded, Static}

object Push {
  implicit val opcode = "push"

  def apply(register: WideRegister)(implicit processorMode: ProcessorMode) =
    R16(register)

  private def R16(register: WideRegister)(implicit processorMode: ProcessorMode) =
    new RegisterEncoded[WideRegister](register, 0x50.toByte :: Nil, opcode, includeRexW = false) {
      override def validate(): Unit = {
        super.validate()
        processorMode match {
          case ProcessorMode.Protected => assume(register.operandByteSize != ValueSize.QuadWord)
          case ProcessorMode.Long => assume(register.operandByteSize != ValueSize.DoubleWord)
          case _ => assume(register.operandByteSize != ValueSize.Byte)
        }
      }
    }

  def apply(operand: ModRMEncodableOperand with FixedSizeOperand)(implicit processorMode: ProcessorMode) =
    RM16(operand)

  private def RM16(operand: ModRMEncodableOperand with FixedSizeOperand)(implicit processorMode: ProcessorMode) =
    new ModRMStatic(operand, 0xFF.toByte :: Nil, 0x06.toByte, opcode) {
      override def validate(): Unit = {
        super.validate()
        processorMode match {
          case ProcessorMode.Protected => assume(operand.operandByteSize != ValueSize.QuadWord)
          case ProcessorMode.Long => assume(operand.operandByteSize != ValueSize.DoubleWord)
          case _ => assume(operand.operandByteSize != ValueSize.Byte)
        }
      }
    }

  def apply(immediate: ImmediateValue)(implicit processorMode: ProcessorMode): Static with Immediate = immediate.operandByteSize match {
    case ValueSize.Byte => Imm8(immediate)
    case ValueSize.Word | ValueSize.DoubleWord => Imm16(immediate)
    case _ => throw new AssertionError
  }

  private def Imm8(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static(0x6A.toByte :: Nil, opcode) with Immediate {
      override def immediate: ImmediateValue = immediateValue
    }

  private def Imm16(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static(0x68.toByte :: Nil, opcode) with Immediate {
      override def immediate: ImmediateValue = immediateValue

      override def validate(): Unit = {
        super.validate()
        assume(immediate.operandByteSize != ValueSize.QuadWord)
      }
    }

  def apply(segment: SegmentRegister)(implicit processorMode: ProcessorMode): Static = segment match {
    case Register.CS => StaticCS()
    case Register.SS => StaticSS()
    case Register.DS => StaticDS()
    case Register.ES => StaticES()
    case Register.FS => StaticFS()
    case Register.GS => StaticGS()
  }

  private def StaticCS()(implicit processorMode: ProcessorMode) = new Static(0x0E.toByte :: Nil, opcode)
  private def StaticSS()(implicit processorMode: ProcessorMode) = new Static(0x16.toByte :: Nil, opcode)
  private def StaticDS()(implicit processorMode: ProcessorMode) = new Static(0x1E.toByte :: Nil, opcode)
  private def StaticES()(implicit processorMode: ProcessorMode) = new Static(0x06.toByte :: Nil, opcode)
  private def StaticFS()(implicit processorMode: ProcessorMode) = new Static(0x0F.toByte :: 0xA0.toByte :: Nil, opcode)
  private def StaticGS()(implicit processorMode: ProcessorMode) = new Static(0x0F.toByte :: 0xA8.toByte :: Nil, opcode)
}

object PushAll {
  implicit val opcode = "pusha"

  def apply()(implicit processorMode: ProcessorMode) = Static()

  private def Static()(implicit processorMode: ProcessorMode) = new Static(0x60.toByte :: Nil, opcode) {
    override def validate(): Unit = {
      super.validate()
      assume(processorMode != ProcessorMode.Long)
    }
  }
}

object PushFlags {
  implicit val opcode = "pushf"

  def apply()(implicit processorMode: ProcessorMode) = Static()

  def Static()(implicit processorMode: ProcessorMode) = new Static(0x9C.toByte :: Nil, opcode)
}
