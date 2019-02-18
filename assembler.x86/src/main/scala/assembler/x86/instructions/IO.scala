package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands.Register.{I386Registers, I8086Registers, X64Registers}
import assembler.x86.operands._
import assembler.x86.operations.OperandInfo.OperandOrder._
import assembler.x86.operations._

object Input extends I8086Registers with I386Registers with X64Registers {
  implicit val opcode: String = "in"

  def apply(immediate: ImmediateValue with ByteSize, destination: AL.type)(implicit processorMode: ProcessorMode): Static =
    Imm8ToAL(immediate)

  def apply(immediate: ImmediateValue with ByteSize, destination: AX.type)(implicit processorMode: ProcessorMode): Static =
    Imm8ToAX(immediate)

  def apply(immediate: ImmediateValue with ByteSize, destination: EAX.type)(implicit processorMode: ProcessorMode): Static =
    Imm8ToEAX(immediate)

  private def Imm8ToAL(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xE4.toByte :: Nil, opcode) with NoDisplacement with Immediate[ByteSize] {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(AL, destination))

      override def immediateOrder: OperandOrder = source

      override val immediate: ImmediateValue with ByteSize = immediateValue
    }

  private def Imm8ToAX(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xE5.toByte :: Nil, opcode) with NoDisplacement with Immediate[ByteSize] {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(AX, destination))

      override def immediateOrder: OperandOrder = source

      override val immediate: ImmediateValue with ByteSize = immediateValue
    }

  private def Imm8ToEAX(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xE5.toByte :: Nil, opcode) with NoDisplacement with Immediate[ByteSize] {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(EAX, destination))

      override def immediateOrder: OperandOrder = source

      override val immediate: ImmediateValue with ByteSize = immediateValue
    }

  def apply(port: Register.DX.type, destination: AL.type)(implicit processorMode: ProcessorMode): Static =
    DXToAL

  def apply(port: Register.DX.type, destination: AX.type)(implicit processorMode: ProcessorMode): Static =
    DXToAX

  def apply(port: Register.DX.type, destination: EAX.type)(implicit processorMode: ProcessorMode): Static =
    DXToEAX

  private def DXToAL()(implicit processorMode: ProcessorMode) = new Static(0xEC.toByte :: Nil, opcode) with NoDisplacement with NoImmediate {
    override protected def implicitInit(): Unit = {
      addOperand(OperandInfo.implicitPort(Register.DX, source))
      addOperand(OperandInfo.implicitOperand(AL, destination))
    }
  }

  private def DXToAX()(implicit processorMode: ProcessorMode) = new Static(0xED.toByte :: Nil, opcode) with NoDisplacement with NoImmediate {
    override protected def implicitInit(): Unit = {
      addOperand(OperandInfo.implicitPort(Register.DX, source))
      addOperand(OperandInfo.implicitOperand(AX, destination))
    }
  }

  private def DXToEAX()(implicit processorMode: ProcessorMode) = new Static(0xED.toByte :: Nil, opcode) with NoDisplacement with NoImmediate {
    override protected def implicitInit(): Unit = {
      addOperand(OperandInfo.implicitPort(Register.DX, source))
      addOperand(OperandInfo.implicitOperand(EAX, destination))
    }
  }
}

object Output extends I8086Registers with I386Registers with X64Registers {
  implicit val opcode: String = "out"

  def apply(destination: AL.type, immediate: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode): Static with Immediate[ByteSize] =
    ALToImm8(immediate)

  def apply(destination: AX.type, immediate: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode): Static with Immediate[ByteSize] =
    AXToImm8(immediate)

  def apply(destination: EAX.type, immediate: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode): Static with Immediate[ByteSize] =
    EAXToImm8(immediate)

  private def ALToImm8(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xE6.toByte :: Nil, opcode) with NoDisplacement with Immediate[ByteSize] {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(AL, source))

      override def immediateOrder: OperandOrder = destination

      override val immediate: ImmediateValue with ByteSize = immediateValue
    }

  private def AXToImm8(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xE7.toByte :: Nil, opcode) with NoDisplacement with Immediate[ByteSize] {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(AX, source))

      override def immediateOrder: OperandOrder = destination

      override val immediate: ImmediateValue with ByteSize = immediateValue
    }

  private def EAXToImm8(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xE7.toByte :: Nil, opcode) with NoDisplacement with Immediate[ByteSize] {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(EAX, source))

      override def immediateOrder: OperandOrder = destination

      override val immediate: ImmediateValue with ByteSize = immediateValue
    }

  def apply(destination: AL.type, port: Register.DX.type)(implicit processorMode: ProcessorMode): Static =
    ALToDX

  def apply(destination: AX.type, port: Register.DX.type)(implicit processorMode: ProcessorMode): Static =
    AXToDX

  def apply(destination: EAX.type, port: Register.DX.type)(implicit processorMode: ProcessorMode): Static =
    EAXToDX

  private def ALToDX()(implicit processorMode: ProcessorMode) =
    new Static(0xEE.toByte :: Nil, opcode) with NoDisplacement with NoImmediate {
      override protected def implicitInit(): Unit = {
        addOperand(OperandInfo.implicitPort(Register.DX, destination))
        addOperand(OperandInfo.implicitOperand(AL, source))
      }
    }

  private def AXToDX()(implicit processorMode: ProcessorMode) =
    new Static(0xEF.toByte :: Nil, opcode) with NoDisplacement with NoImmediate  {
      override protected def implicitInit(): Unit = {
        addOperand(OperandInfo.implicitPort(Register.DX, destination))
        addOperand(OperandInfo.implicitOperand(AX, source))
      }
    }

  private def EAXToDX()(implicit processorMode: ProcessorMode) =
    new Static(0xEF.toByte :: Nil, opcode) with NoDisplacement with NoImmediate  {
      override protected def implicitInit(): Unit = {
        addOperand(OperandInfo.implicitPort(Register.DX, destination))
        addOperand(OperandInfo.implicitOperand(EAX, source))
      }
    }
}
