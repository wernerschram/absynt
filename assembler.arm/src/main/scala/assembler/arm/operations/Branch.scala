package assembler.arm.operations

import assembler.Label
import assembler.arm.operands.Condition._
import assembler.arm.operands.RelativePointer
import assembler.arm.operands.registers.GeneralRegister
import assembler.sections.Section

class BranchImmediate(val label: Label, destination: RelativePointer, val condition: Condition, val code: Byte, val opcode: String)
  extends Conditional {
  override def encodeWord()(implicit page: Section): Int =
    super.encodeWord() | ((code & 0xF0) << 20) | destination.encode

  override def toString = s"${super.toString()} ${destination.toString}"
}

class BranchRegister(val label: Label, destination: GeneralRegister, val condition: Condition, val code: Byte, val opcode: String)
  extends Conditional {
  override def encodeWord()(implicit page: Section): Int =
    super.encodeWord() | 0x012FFF00 | ((code & 0x0F) << 4) | destination.registerCode

  override def toString = s"${super.toString()} ${destination.toString}"
}
