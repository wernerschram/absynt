package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.opcodes.BranchImmediate
import assembler.arm.opcodes.BranchRegister
import assembler.arm.operands.Condition._
import assembler.arm.operands.RelativePointer
import assembler.arm.operands.registers.GeneralRegister

class Branch(code: Byte, val opcode: String) {
  private val Immediate = new BranchImmediate(code)(opcode)

  def apply(destination: RelativePointer, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Immediate(destination, condition)
}

class BranchExchange(registerCode: Byte, val opcode: String) {
  // TODO HBit
  private val Register = new BranchRegister(registerCode)(opcode)

  def apply(destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Register(destination, condition)
}

class BranchLinkExchange(immediateCode: Byte, registerCode: Byte, opcode: String) extends BranchExchange(registerCode, opcode) {
  // TODO HBit
  private val Immediate = new BranchImmediate(immediateCode)(opcode)

  def apply(destination: RelativePointer)(implicit processorMode: ProcessorMode) =
    Immediate(destination, Unpredictable)

}

object Branch extends Branch(0xA0.toByte, "b")
object BranchLink extends Branch(0xB0.toByte, "bl")
object BranchExchange extends BranchExchange(0x1.toByte, "bx")
object BranchLinkExchange extends BranchLinkExchange(0xA0.toByte, 0x3.toByte, "blx")
object BranchExchangeJazelle extends BranchExchange(0x2.toByte, "bxj")
