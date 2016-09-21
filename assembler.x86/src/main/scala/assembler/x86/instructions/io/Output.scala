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

  def apply(destination: AccumulatorRegister, immediate: ImmediateValue)(implicit processorMode: ProcessorMode) = {
    assume(destination == Register.AL || destination == Register.AX)
    assume(immediate.operandByteSize == 1)
    (destination) match {
      case (Register.AL) => ALToImm8(immediate)
      case (Register.AX) => AXToImm8(immediate)
    }
  }
  def apply(destination: AccumulatorRegister, port: DataRegister)(implicit processorMode: ProcessorMode) = {
    assume(destination == Register.AL || destination == Register.AX || destination == Register.EAX)
    assume(port == Register.DX)
    (destination) match {
      case (Register.AL) => ALToDX()
      case (Register.AX | Register.EAX) => AXToDX()
    }
  }
}