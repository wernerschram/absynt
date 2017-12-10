package assembler.arm.operations

import assembler.Label
import assembler.arm.operands.Condition._
import assembler.arm.operands.{ArmRelativeOffset, RelativeA32Pointer, RelativePointer, RelativeThumbPointer}
import assembler.arm.operands.registers.GeneralRegister

class BranchImmediate[AddressType<:RelativePointer](label: Label, destination: AddressType, val condition: Condition, val code: Byte, val opcode: String)
  extends Conditional(label) {
  override def encodeWord: Int =
    super.encodeWord | ((code & 0xF0) << 20) | destination.encode

  //override def toString = s"$labelPrefix$mnemonicString ${(destination + ArmOffset(8)).toString}"
  override def toString = {
    destination match {
      case p: RelativeA32Pointer =>
        s"$labelPrefix$mnemonicString ${p.toString}"
      case p: RelativeThumbPointer =>
        s"$labelPrefix$mnemonicString ${p.toString}"
    }
  }
}

class BranchRegister(label: Label, destination: GeneralRegister, val condition: Condition, val code: Byte, val opcode: String)
  extends Conditional(label) {
  override def encodeWord: Int =
    super.encodeWord | 0x012FFF00 | ((code & 0x0F) << 4) | destination.registerCode

  override def toString = s"$labelPrefix$mnemonicString ${destination.toString}"
}
