package assembler.arm.operations

import assembler.memory.MemoryPage
import assembler.arm.operands.Condition._
import assembler.arm.operands.Operand
import assembler.arm.operands.RelativePointer
import assembler.arm.operands.registers.GeneralRegister

class BranchImmediateOperation(val code: Byte)(implicit mnemonic: String)
    extends Operation(mnemonic) {

  def apply(destination: RelativePointer, condition: Condition) = new ConditionalARMOperation(condition) {

    override def encodeWord()(implicit page: MemoryPage) =
      // TODO: apply lBit
      (super.encodeWord() | ((code & 0xF0) << 20) | (destination.encode))
    override def toString = s"${BranchImmediateOperation.this.mnemonic}${condition.mnemonicExtension} ${destination.toString}"
  }
}

class BranchImmediate(destination: RelativePointer, condition: Condition, val code: Byte, val mnemonic: String)
extends ConditionalARMOperation(condition) {
    override def encodeWord()(implicit page: MemoryPage) =
      // TODO: apply lBit
      (super.encodeWord() | ((code & 0xF0) << 20) | (destination.encode))
    override def toString = s"${mnemonic}${condition.mnemonicExtension} ${destination.toString}"
}

class BranchRegister(val code: Byte)(implicit mnemonic: String)
    extends Operation(mnemonic) {
  def apply(destination: GeneralRegister, condition: Condition) = new ConditionalARMOperation(condition) {

    override def encodeWord()(implicit page: MemoryPage) =
      // TODO: apply lBit
      (super.encodeWord() | 0x012FFF00 | ((code & 0x0F) << 4) | (destination.registerCode))
    override def toString = s"${BranchRegister.this.mnemonic}${condition.mnemonicExtension} ${destination.toString}"

  }

}