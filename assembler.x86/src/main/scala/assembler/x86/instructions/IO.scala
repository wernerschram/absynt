package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands.AccumulatorRegister
import assembler.x86.operands.DataRegister
import assembler.x86.operands.ImmediateValue
import assembler.x86.operands.Register
import assembler.x86.operations.Immediate
import assembler.x86.operations.ReversedOperands
import assembler.x86.operations.Static
import assembler.x86.operands.ValueSize

object Input {
  implicit val opcode = "in"

  private def Imm8ToAL(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static(0xE4.toByte :: Nil, opcode) with Immediate with ReversedOperands {
      override def operands = Register.AL :: super.operands
      override val immediate = immediateValue
      override def validate = {
        super.validate
        assume(immediate.operandByteSize == ValueSize.Byte)
      }
    }
  private def Imm8ToAX(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static(0xE5.toByte :: Nil, opcode) with Immediate with ReversedOperands {
      override def operands = Register.AX :: super.operands
      override val immediate = immediateValue
      override def validate = {
        super.validate
        assume(immediate.operandByteSize == ValueSize.Byte)
      }
    }

  private def DXToAL()(implicit processorMode: ProcessorMode) = new Static(0xEC.toByte :: Nil, opcode) {
    override def operands = Register.DX :: Register.AL :: Nil
    override def operandSize = Register.AL.operandByteSize
  }
  private def DXToAX()(implicit processorMode: ProcessorMode) = new Static(0xED.toByte :: Nil, opcode) {
    override def operands = Register.DX :: Register.AX :: Nil
    override def operandSize = Register.AX.operandByteSize
  }
  private def DXToEAX()(implicit processorMode: ProcessorMode) = new Static(0xED.toByte :: Nil, opcode) {
    override def operands = Register.DX :: Register.EAX :: Nil
    override def operandSize = Register.EAX.operandByteSize
  }

  def apply(immediate: ImmediateValue, destination: AccumulatorRegister)(implicit processorMode: ProcessorMode) = {
    assume(immediate.operandByteSize == ValueSize.Byte)
    (destination) match {
      case (Register.AL) => Imm8ToAL(immediate)
      case (Register.AX) => Imm8ToAX(immediate)
      case default => throw new AssertionError
    }
  }

  def apply(port: DataRegister, destination: AccumulatorRegister)(implicit processorMode: ProcessorMode) = {
    assume(port == Register.DX)
    (destination) match {
      case (Register.AL) => DXToAL
      case (Register.AX) => DXToAX
      case (Register.EAX) => DXToEAX
      case default => throw new AssertionError
    }
  }
}

object Output {
  implicit val opcode = "out"

  private def ALToImm8(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static(0xE6.toByte :: Nil, opcode) with Immediate {
      override def operands = Register.AL :: super.operands
      override val immediate = immediateValue
      override def validate = {
        super.validate
        assume(immediate.operandByteSize == ValueSize.Byte)
      }
    }
  private def AXToImm8(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static(0xE7.toByte :: Nil, opcode) with Immediate {
      override def operands = Register.AX :: super.operands
      override val immediate = immediateValue
      override def validate = {
        super.validate
        assume(immediate.operandByteSize == ValueSize.Byte)
      }
    }

  private def ALToDX()(implicit processorMode: ProcessorMode) =
    new Static(0xEE.toByte :: Nil, opcode) with ReversedOperands {
      override def operands = Register.DX :: Register.AL :: Nil
      override def operandSize = Register.AL.operandByteSize
    }

  private def AXToDX()(implicit processorMode: ProcessorMode) =
    new Static(0xEF.toByte :: Nil, opcode) with ReversedOperands {
      override def operands = Register.DX :: Register.AX :: Nil
      override def operandSize = Register.AX.operandByteSize
    }

  private def EAXToDX()(implicit processorMode: ProcessorMode) =
    new Static(0xEF.toByte :: Nil, opcode) with ReversedOperands {
      override def operands = Register.DX :: Register.EAX :: Nil
      override def operandSize = Register.EAX.operandByteSize
    }

  def apply(destination: AccumulatorRegister, immediate: ImmediateValue)(implicit processorMode: ProcessorMode) = {
    assume(immediate.operandByteSize == ValueSize.Byte)
    (destination) match {
      case (Register.AL) => ALToImm8(immediate)
      case (Register.AX) => AXToImm8(immediate)
      case default => throw new AssertionError
    }
  }
  def apply(destination: AccumulatorRegister, port: DataRegister)(implicit processorMode: ProcessorMode) = {
    assume(port == Register.DX)
    (destination) match {
      case (Register.AL) => ALToDX()
      case (Register.AX) => AXToDX()
      case (Register.EAX) => EAXToDX()
      case default => throw new AssertionError
    }
  }
}
