package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operations.OperandInfo.OperandOrder._
import assembler.x86.operations._

object Input {
  implicit val opcode: String = "in"

  def apply(immediate: ImmediateValue with ByteSize, destination: Register.AL.type)(implicit processorMode: ProcessorMode): Static =
    Imm8ToAL(immediate)

  def apply(immediate: ImmediateValue with ByteSize, destination: Register.AX.type)(implicit processorMode: ProcessorMode): Static =
    Imm8ToAX(immediate)

  def apply(immediate: ImmediateValue with ByteSize, destination: Register.EAX.type)(implicit processorMode: ProcessorMode): Static =
    Imm8ToEAX(immediate)

  private def Imm8ToAL(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xE4.toByte :: Nil, opcode) with NoDisplacement with Immediate {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.AL, destination))

      override def immediateOrder: OperandOrder = source

      override val immediate: ImmediateValue = immediateValue
    }

  private def Imm8ToAX(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xE5.toByte :: Nil, opcode) with NoDisplacement with Immediate {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.AX, destination))

      override def immediateOrder: OperandOrder = source

      override val immediate: ImmediateValue = immediateValue
    }

  private def Imm8ToEAX(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xE5.toByte :: Nil, opcode) with NoDisplacement with Immediate {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.EAX, destination))

      override def immediateOrder: OperandOrder = source

      override val immediate: ImmediateValue = immediateValue
    }

  def apply(port: Register.DX.type, destination: Register.AL.type)(implicit processorMode: ProcessorMode): Static =
    DXToAL

  def apply(port: Register.DX.type, destination: Register.AX.type)(implicit processorMode: ProcessorMode): Static =
    DXToAX

  def apply(port: Register.DX.type, destination: Register.EAX.type)(implicit processorMode: ProcessorMode): Static =
    DXToEAX

  private def DXToAL()(implicit processorMode: ProcessorMode) = new Static(0xEC.toByte :: Nil, opcode) with NoDisplacement with NoImmediate {
    override protected def implicitInit(): Unit = {
      addOperand(OperandInfo.implicitPort(Register.DX, source))
      addOperand(OperandInfo.implicitOperand(Register.AL, destination))
    }
  }

  private def DXToAX()(implicit processorMode: ProcessorMode) = new Static(0xED.toByte :: Nil, opcode) with NoDisplacement with NoImmediate {
    override protected def implicitInit(): Unit = {
      addOperand(OperandInfo.implicitPort(Register.DX, source))
      addOperand(OperandInfo.implicitOperand(Register.AX, destination))
    }
  }

  private def DXToEAX()(implicit processorMode: ProcessorMode) = new Static(0xED.toByte :: Nil, opcode) with NoDisplacement with NoImmediate {
    override protected def implicitInit(): Unit = {
      addOperand(OperandInfo.implicitPort(Register.DX, source))
      addOperand(OperandInfo.implicitOperand(Register.EAX, destination))
    }
  }
}

object Output {
  implicit val opcode: String = "out"

  def apply(destination: Register.AL.type, immediate: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode): Static with Immediate =
    ALToImm8(immediate)

  def apply(destination: Register.AX.type, immediate: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode): Static with Immediate =
    AXToImm8(immediate)

  def apply(destination: Register.EAX.type, immediate: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode): Static with Immediate =
    EAXToImm8(immediate)

  private def ALToImm8(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xE6.toByte :: Nil, opcode) with NoDisplacement with Immediate {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.AL, source))

      override def immediateOrder: OperandOrder = destination

      override val immediate: ImmediateValue = immediateValue
    }

  private def AXToImm8(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xE7.toByte :: Nil, opcode) with NoDisplacement with Immediate {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.AX, source))

      override def immediateOrder: OperandOrder = destination

      override val immediate: ImmediateValue = immediateValue
    }

  private def EAXToImm8(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xE7.toByte :: Nil, opcode) with NoDisplacement with Immediate {
      override protected def implicitInit(): Unit =
        addOperand(OperandInfo.implicitOperand(Register.EAX, source))

      override def immediateOrder: OperandOrder = destination

      override val immediate: ImmediateValue = immediateValue
    }

  def apply(destination: Register.AL.type, port: Register.DX.type)(implicit processorMode: ProcessorMode): Static =
    ALToDX

  def apply(destination: Register.AX.type, port: Register.DX.type)(implicit processorMode: ProcessorMode): Static =
    AXToDX

  def apply(destination: Register.EAX.type, port: Register.DX.type)(implicit processorMode: ProcessorMode): Static =
    EAXToDX

  private def ALToDX()(implicit processorMode: ProcessorMode) =
    new Static(0xEE.toByte :: Nil, opcode) with NoDisplacement with NoImmediate {
      override protected def implicitInit(): Unit = {
        addOperand(OperandInfo.implicitPort(Register.DX, destination))
        addOperand(OperandInfo.implicitOperand(Register.AL, source))
      }
    }

  private def AXToDX()(implicit processorMode: ProcessorMode) =
    new Static(0xEF.toByte :: Nil, opcode) with NoDisplacement with NoImmediate  {
      override protected def implicitInit(): Unit = {
        addOperand(OperandInfo.implicitPort(Register.DX, destination))
        addOperand(OperandInfo.implicitOperand(Register.AX, source))
      }
    }

  private def EAXToDX()(implicit processorMode: ProcessorMode) =
    new Static(0xEF.toByte :: Nil, opcode) with NoDisplacement with NoImmediate  {
      override protected def implicitInit(): Unit = {
        addOperand(OperandInfo.implicitPort(Register.DX, destination))
        addOperand(OperandInfo.implicitOperand(Register.EAX, source))
      }
    }
}
