package assembler.x86.instructions.io

import assembler.x86.ProcessorMode
import assembler.x86.operands.ImmediateValue
import assembler.x86.operands._
import assembler.x86.operations.Static
import assembler.x86.operations.Immediate

object Output {
  implicit val opcode = "out"

//  private val ALToImm8 = new Static(0xE6.toByte :: Nil).withImmediate({ case (value, _) => value.operandByteSize == 1 })
//  private val AXToImm8 = new Static(0xE7.toByte :: Nil).withImmediate({ case (value, _) => value.operandByteSize == 1 })

  private def ALToImm8(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) = new Static(0xE6.toByte :: Nil, opcode) with Immediate {
    override val immediate = immediateValue
    override def validate = {
      super.validate
      assume(immediate.operandByteSize == 1)
    }
  }
  private def AXToImm8(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) = new Static(0xE7.toByte :: Nil, opcode) with Immediate {
    override val immediate = immediateValue
    override def validate = {
      super.validate
      assume(immediate.operandByteSize == 1)
    }
  }

  private def ALToDX()(implicit processorMode: ProcessorMode) = new Static(0xEE.toByte :: Nil, opcode) {
    override def operands = Register.DX :: Register.AL :: Nil
    override def operandSize = Some(Register.AL.operandByteSize)
  }
  private def AXToDX()(implicit processorMode: ProcessorMode) = new Static(0xEF.toByte :: Nil, opcode) {
    override def operands = Register.DX :: Register.AX :: Nil
    override def operandSize = Some(Register.AX.operandByteSize)
  }
  private def EAXToDX()(implicit processorMode: ProcessorMode) = new Static(0xEF.toByte :: Nil, opcode) {
    override def operands = Register.DX :: Register.EAX :: Nil
    override def operandSize = Some(Register.EAX.operandByteSize)
  }

//  private val ALToDX = new Static(0xEE.toByte :: Nil).withImplicitRegisters(Register.AL, Register.DX)
//  private val AXToDX = new Static(0xEF.toByte :: Nil).withImplicitRegisters(Register.AL, Register.DX)

  def apply(destination: AccumulatorRegister, immediate: ImmediateValue)(implicit processorMode: ProcessorMode) = {
    assume(destination == Register.AL || destination == Register.AX)
    assume(immediate.operandByteSize == 1)
    (destination) match {
      case (Register.AL) => ALToImm8(immediate)
      case (Register.AX) => AXToImm8(immediate)
      case default => throw new AssertionError
    }
  }
  def apply(destination: AccumulatorRegister, port: DataRegister)(implicit processorMode: ProcessorMode) = {
    assume(destination == Register.AL || destination == Register.AX || destination == Register.EAX)
    assume(port == Register.DX)
    (destination) match {
      case (Register.AL) => ALToDX()
      case (Register.AX) => AXToDX()
      case (Register.EAX) => EAXToDX()
      case default => throw new AssertionError
    }
  }
}