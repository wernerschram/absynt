package assembler.arm.operations

import assembler.memory.MemoryPage
import assembler.arm.operands.Condition._
import assembler.arm.operands.Operand
import assembler.arm.operands.RelativePointer
import assembler.arm.operands.registers.GeneralRegister

class BranchImmediate(destination: RelativePointer, val condition: Condition, val code: Byte, val opcode: String)
    extends Conditional {
  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | ((code & 0xF0) << 20) | (destination.encode))
  override def toString = s"${super.toString()} ${destination.toString}"
}

class BranchRegister(destination: GeneralRegister, val condition: Condition, val code: Byte, val opcode: String)
    extends Conditional {
  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | 0x012FFF00 | ((code & 0x0F) << 4) | (destination.registerCode))
  override def toString = s"${super.toString()} ${destination.toString}"
}
