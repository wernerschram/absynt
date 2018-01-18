package assembler.arm.operations

import assembler.arm.operands.Condition._
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operands.{RelativeA32Pointer, RelativePointer, RelativeThumbPointer}

class BranchImmediate[AddressType<:RelativePointer](destination: AddressType, val condition: Condition, val code: Byte, val opcode: String)
  extends Conditional {
  override def encodeWord: Int =
    super.encodeWord | ((code & 0xF0) << 20) | destination.encode

  override def toString: String = {
    destination match {
      case p: RelativeA32Pointer =>
        s"$mnemonicString ${p.toString}"
      case p: RelativeThumbPointer =>
        s"$mnemonicString ${p.toString}"
    }
  }
}

class BranchRegister(destination: GeneralRegister, val condition: Condition, val code: Byte, val opcode: String)
  extends Conditional {
  override def encodeWord: Int =
    super.encodeWord | 0x012FFF00 | ((code & 0x0F) << 4) | destination.registerCode

  override def toString = s"$mnemonicString ${destination.toString}"
}
