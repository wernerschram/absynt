package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operations._
import assembler.x86.operations.OperandInfo.OperandOrder._

object Push {
  implicit val opcode: String = "push"

  def apply(register: WideRegister)(implicit processorMode: ProcessorMode): RegisterEncoded[WideRegister] =
    (register, processorMode) match {
      case (r: WordSize, _) =>
        R16(r)
      case (r: QuadWordSize, ProcessorMode.Long) =>
        R16(r)
      case (r: DoubleWordSize, ProcessorMode.Protected | ProcessorMode.Real) =>
        R16(r)
      case _ =>
        throw new AssertionError
    }

  private def R16(register: WideRegister)(implicit processorMode: ProcessorMode) =
    new RegisterEncoded[WideRegister](register, Seq(0x50.toByte), opcode, includeRexW = false) {
      override def registerOrder: OperandOrder = destination
    }

  def apply(operand: ModRMEncodableOperand with WideSize)(implicit processorMode: ProcessorMode): ModRMStatic =
    (processorMode, operand) match {
      case (_, o: WordSize) =>
        RM16(o)
      case (ProcessorMode.Protected | ProcessorMode.Real, o: DoubleWordSize) =>
        RM16(o)
      case (ProcessorMode.Long, o: QuadWordSize) =>
        RM16(o)
      case _ =>
        throw new AssertionError
    }

  private def RM16(operand: ModRMEncodableOperand with WideSize)(implicit processorMode: ProcessorMode) =
    new ModRMStatic(operand, 0xFF.toByte :: Nil, 0x06.toByte, opcode) {
      override def operandRMOrder: OperandOrder = destination
    }

  def apply(immediate: ImmediateValue)(implicit processorMode: ProcessorMode): Static with Immediate =
    immediate match {
    case i: ByteSize => Imm8(i)
    case i: ExtendedSize => Imm16(i)
    case _ => throw new AssertionError
  }

  private def Imm8(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0x6A.toByte :: Nil, opcode) with Immediate {
      override def immediate: ImmediateValue = immediateValue

      override def immediateOrder: OperandOrder = destination
    }

  private def Imm16(immediateValue: ImmediateValue with ExtendedSize)(implicit processorMode: ProcessorMode) =
    new Static(0x68.toByte :: Nil, opcode) with Immediate {
      override def immediate: ImmediateValue = immediateValue

      override def immediateOrder: OperandOrder = destination
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
  implicit val opcode: String = "pusha"

  def apply()(implicit processorMode: ProcessorMode): Static = Static()

  private def Static()(implicit processorMode: ProcessorMode) = new Static(0x60.toByte :: Nil, opcode) {
    override def operands = Set(OperandInfo.allRegisters)
  }
}

object PushFlags {
  implicit val opcode: String = "pushf"

  def apply()(implicit processorMode: ProcessorMode): Static = Static()

  def Static()(implicit processorMode: ProcessorMode) = new Static(0x9C.toByte :: Nil, opcode)
}
