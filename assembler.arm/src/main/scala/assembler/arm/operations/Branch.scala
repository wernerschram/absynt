package assembler.arm.operations

import assembler.memory.MemoryPage
import assembler.arm.operands.Condition._
import assembler.arm.operands.Operand
import assembler.arm.operands.RelativePointer
import assembler.arm.operands.registers.GeneralRegister

class BranchImmediate(val code: Byte)(implicit mnemonic: String)
    extends Opcode(mnemonic) {

  def apply(destination: RelativePointer, condition: Condition) = new ConditionalARMInstruction(condition) {

    override def encodeWord()(implicit page: MemoryPage) =
      // TODO: apply lBit
      (super.encodeWord() | ((code & 0xF0) << 20) | (destination.encode))
    override def toString = s"${BranchImmediate.this.mnemonic}${condition.mnemonicExtension} ${destination.toString}"
  }
}

class BranchRegister(val code: Byte)(implicit mnemonic: String)
    extends Opcode(mnemonic) {
  def apply(destination: GeneralRegister, condition: Condition) = new ConditionalARMInstruction(condition) {

    override def encodeWord()(implicit page: MemoryPage) =
      // TODO: apply lBit
      (super.encodeWord() | 0x012FFF00 | ((code & 0x0F) << 4) | (destination.registerCode))
    override def toString = s"${BranchRegister.this.mnemonic}${condition.mnemonicExtension} ${destination.toString}"

  }

}