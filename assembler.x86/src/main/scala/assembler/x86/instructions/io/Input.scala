package assembler.x86.instructions.io

import assembler.x86.ProcessorMode
import assembler.x86.operands.AccumulatorRegister
import assembler.x86.operands.DataRegister
import assembler.x86.operands.ImmediateValue
import assembler.x86.operands.Register
import assembler.x86.operations.Immediate
import assembler.x86.operations.ReversedOperands
import assembler.x86.operations.Static

object Input {
  implicit val opcode = "in"

  private def Imm8ToAL(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static(0xE4.toByte :: Nil, opcode) with Immediate with ReversedOperands {
      override def operands = Register.AL :: super.operands
      override val immediate = immediateValue
      override def validate = {
        super.validate
        assume(immediate.operandByteSize == 1)
      }
    }
  private def Imm8ToAX(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static(0xE5.toByte :: Nil, opcode) with Immediate with ReversedOperands {
      override def operands = Register.AX :: super.operands
      override val immediate = immediateValue
      override def validate = {
        super.validate
        assume(immediate.operandByteSize == 1)
      }
    }

  private def DXToAL()(implicit processorMode: ProcessorMode) = new Static(0xEC.toByte :: Nil, opcode) {
    override def operands = Register.DX :: Register.AL :: Nil
    override def operandSize = Some(Register.AL.operandByteSize)
  }
  private def DXToAX()(implicit processorMode: ProcessorMode) = new Static(0xED.toByte :: Nil, opcode) {
    override def operands = Register.DX :: Register.AX :: Nil
    override def operandSize = Some(Register.AX.operandByteSize)
  }
  private def DXToEAX()(implicit processorMode: ProcessorMode) = new Static(0xED.toByte :: Nil, opcode) {
    override def operands = Register.DX :: Register.EAX :: Nil
    override def operandSize = Some(Register.EAX.operandByteSize)
  }

  def apply(immediate: ImmediateValue, destination: AccumulatorRegister)(implicit processorMode: ProcessorMode) = {
    assume(immediate.operandByteSize == 1)
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