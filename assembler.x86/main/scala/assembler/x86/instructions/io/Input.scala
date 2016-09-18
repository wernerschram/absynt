package assembler.x86.instructions.io

import assembler.x86.ProcessorMode
import assembler.x86.opcodes.Static
import assembler.x86.operands.ImmediateValue
import assembler.x86.operands.registers._

object Input {
  implicit val opcode = "in"

  private val Imm8ToAL = new Static(0xE4.toByte :: Nil).withImmediate({ case (value, _) => value.operandByteSize == 1 })
  private val Imm8ToAX = new Static(0xE5.toByte :: Nil).withImmediate({ case (value, _) => value.operandByteSize == 1 })

  private val DXToAL = (new Static(0xEC.toByte :: Nil)).asTwoOperandOpcode[AccumulatorRegister, DataRegister]({ case (Register.AL, Register.DX, _) => true; case _ => false })
  private val DXToAX = (new Static(0xED.toByte :: Nil)).asTwoOperandOpcode[AccumulatorRegister, DataRegister]({ case (Register.AX | Register.EAX, Register.DX, _) => true; case _ => false })


  def apply(immediate: ImmediateValue, destination: AccumulatorRegister)(implicit processorMode: ProcessorMode) = (destination, immediate.operandByteSize) match {
    case (Register.AL, 1) => Imm8ToAL(immediate)
    case (Register.AX, 1) => Imm8ToAX(immediate)
    case _ => throw new Exception // TODO: replace with correct exception
  }
  
  def apply(port: DataRegister, destination: AccumulatorRegister)(implicit processorMode: ProcessorMode) = (destination, port) match {
    case (Register.AL, Register.DX) => DXToAL(destination, port)
    case (Register.AX | Register.EAX, Register.DX) => DXToAX(destination, port)
    case _ => throw new Exception // TODO: replace with correct exception
  }
}