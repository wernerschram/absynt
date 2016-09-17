package assembler.x86.instructions.io

import assembler.x86.ProcessorMode
import assembler.x86.opcodes.Static
import assembler.x86.operands.ImmediateValue
import assembler.x86.operands.registers._

object Output {
  implicit val opcode = "out"

  private val ALToImm8 = new Static(0xE6.toByte :: Nil).withImmediate({ case (value, _) => value.operandByteSize == 1 })
  private val AXToImm8 = new Static(0xE7.toByte :: Nil).withImmediate({ case (value, _) => value.operandByteSize == 1 })

  private val ALToDX = new Static(0xEE.toByte :: Nil).withImplicitRegisters(Register.AL, Register.DX)
  private val AXToDX = new Static(0xEF.toByte :: Nil).withImplicitRegisters(Register.AL, Register.DX)

  
  def apply(destination: AccumulatorRegister, immediate: ImmediateValue)(implicit processorMode: ProcessorMode) = (destination, immediate.operandByteSize) match {
    case (Register.AL, 1) => ALToImm8(immediate)
    case (Register.AX, 1) => AXToImm8(immediate)
    case _ => throw new Exception // TODO: replace with correct exception
  }
  def apply(destination: AccumulatorRegister, port: DataRegister)(implicit processorMode: ProcessorMode) = (destination, port) match {
    case (Register.AL, Register.DX) => ALToDX()
    case (Register.AX | Register.EAX, Register.DX) => AXToDX()
    case _ => throw new Exception // TODO: replace with correct exception
  }
}