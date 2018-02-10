package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operations.{Immediate, OperandInfo, ReversedOperands, Static}
import assembler.x86.operations.OperandInfo.OperandOrder._

object Input {
  implicit val opcode: String = "in"

  def apply(immediate: ImmediateValue, destination: AccumulatorRegister)(implicit processorMode: ProcessorMode): Static
    with Immediate with ReversedOperands = {
    assume(immediate.operandByteSize == ValueSize.Byte)
    destination match {
      case (Register.AL) => Imm8ToAL(immediate)
      case (Register.AX) => Imm8ToAX(immediate)
      case _ => throw new AssertionError
    }
  }

  private def Imm8ToAL(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static(0xE4.toByte :: Nil, opcode) with Immediate with ReversedOperands {
      override def operands: Seq[OperandInfo] = OperandInfo.implicitOperand(Register.AL, second) +: super.operands

      override def immediateOrder: OperandOrder = first

      override val immediate: ImmediateValue = immediateValue

      override def validate(): Unit = {
        super.validate()
        assume(immediate.operandByteSize == ValueSize.Byte)
      }

    }

  private def Imm8ToAX(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static(0xE5.toByte :: Nil, opcode) with Immediate with ReversedOperands {
      override def operands: Seq[OperandInfo] = OperandInfo.implicitOperand(Register.AX, second) +: super.operands

      override def immediateOrder: OperandOrder = first

      override val immediate: ImmediateValue = immediateValue

      override def validate(): Unit = {
        super.validate()
        assume(immediate.operandByteSize == ValueSize.Byte)
      }
    }

  def apply(port: DataRegister, destination: AccumulatorRegister)(implicit processorMode: ProcessorMode): Static = {
    assume(port == Register.DX)
    destination match {
      case (Register.AL) => DXToAL
      case (Register.AX) => DXToAX
      case (Register.EAX) => DXToEAX
      case _ => throw new AssertionError
    }
  }

  private def DXToAL()(implicit processorMode: ProcessorMode) = new Static(0xEC.toByte :: Nil, opcode) {
    override def operands: Seq[OperandInfo] = Seq(OperandInfo.implicitOperand(Register.DX, second), OperandInfo.implicitOperand(Register.AL, first))

    override def operandSize: OperandSize = Register.AL.operandByteSize
  }

  private def DXToAX()(implicit processorMode: ProcessorMode) = new Static(0xED.toByte :: Nil, opcode) {
    override def operands: Seq[OperandInfo] = Seq(OperandInfo.implicitOperand(Register.DX, second), OperandInfo.implicitOperand(Register.AX, first))

    override def operandSize: OperandSize = Register.AX.operandByteSize
  }

  private def DXToEAX()(implicit processorMode: ProcessorMode) = new Static(0xED.toByte :: Nil, opcode) {
    override def operands: Seq[OperandInfo] = Seq(OperandInfo.implicitOperand(Register.DX, second), OperandInfo.implicitOperand(Register.EAX, first))

    override def operandSize: OperandSize = Register.EAX.operandByteSize
  }
}

object Output {
  implicit val opcode: String = "out"

  def apply(destination: AccumulatorRegister, immediate: ImmediateValue)(implicit processorMode: ProcessorMode): Static with Immediate = {
    assume(immediate.operandByteSize == ValueSize.Byte)
    destination match {
      case (Register.AL) => ALToImm8(immediate)
      case (Register.AX) => AXToImm8(immediate)
      case _ => throw new AssertionError
    }
  }

  private def ALToImm8(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static(0xE6.toByte :: Nil, opcode) with Immediate {
      override def operands: Seq[OperandInfo] = OperandInfo.implicitOperand(Register.AL, second) +: super.operands

      override def immediateOrder: OperandOrder = first

      override val immediate: ImmediateValue = immediateValue

      override def validate(): Unit = {
        super.validate()
        assume(immediate.operandByteSize == ValueSize.Byte)
      }
    }

  private def AXToImm8(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static(0xE7.toByte :: Nil, opcode) with Immediate {
      override def operands: Seq[OperandInfo] = OperandInfo.implicitOperand(Register.AX, second) +: super.operands

      override def immediateOrder: OperandOrder = first

      override val immediate: ImmediateValue = immediateValue

      override def validate(): Unit = {
        super.validate()
        assume(immediate.operandByteSize == ValueSize.Byte)
      }
    }

  def apply(destination: AccumulatorRegister, port: DataRegister)(implicit processorMode: ProcessorMode): Static with ReversedOperands = {
    assume(port == Register.DX)
    destination match {
      case (Register.AL) => ALToDX()
      case (Register.AX) => AXToDX()
      case (Register.EAX) => EAXToDX()
      case _ => throw new AssertionError
    }
  }

  private def ALToDX()(implicit processorMode: ProcessorMode) =
    new Static(0xEE.toByte :: Nil, opcode) with ReversedOperands {
      override def operands: Seq[OperandInfo] = Seq(OperandInfo.implicitOperand(Register.DX, first), OperandInfo.implicitOperand(Register.AL, second))

      override def operandSize: OperandSize = Register.AL.operandByteSize
    }

  private def AXToDX()(implicit processorMode: ProcessorMode) =
    new Static(0xEF.toByte :: Nil, opcode) with ReversedOperands {
      override def operands: Seq[OperandInfo] = Seq(OperandInfo.implicitOperand(Register.DX, first), OperandInfo.implicitOperand(Register.AX, second))

      override def operandSize: OperandSize = Register.AX.operandByteSize
    }

  private def EAXToDX()(implicit processorMode: ProcessorMode) =
    new Static(0xEF.toByte :: Nil, opcode) with ReversedOperands {
      override def operands: Seq[OperandInfo] = Seq(OperandInfo.implicitOperand(Register.DX, first), OperandInfo.implicitOperand(Register.EAX, second))

      override def operandSize: OperandSize = Register.EAX.operandByteSize
    }
}
