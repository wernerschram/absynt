package assembler.arm.opcodes

import scala.language.implicitConversions

import assembler.arm.ProcessorMode
import assembler.arm.instructions.ARMInstruction
import assembler.arm.instructions.ConditionalARMInstruction
import assembler.arm.operands.Condition.Condition
import assembler.arm.operands.registers._
import assembler.memory.MemoryPage

class MoveFromStatusRegister()(implicit mnemonic: String)
    extends Opcode(mnemonic) {

  def apply(source: StatusRegister, destination: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode): ARMInstruction =
    new ConditionalARMInstruction(condition) {
      override def encodeWord()(implicit page: MemoryPage) =
        (super.encodeWord() | 0x010f0000 | (source.registerCode << 22) | (destination.registerCode << 12))

      override val toString = s"${mnemonic}${condition.mnemonicExtension} ${destination.toString}, ${source.toString}"
    }
}


object Fields extends Enumeration {
  type Fields = Value
  
  val control = Value(16, "c")
  val extension = Value(17, "x")
  val status = Value(18, "s")
  val flags = Value(19, "f")

  implicit def fieldsToString(set: ValueSet) : String = {
    set.foldRight("")((a,b) => a + b).reverse
  }
}

class MoveToStatusRegister()(implicit mnemonic: String)
    extends Opcode(mnemonic) {

  def apply(source: GeneralRegister, destination: StatusRegister, fields: Fields.ValueSet, condition: Condition)(implicit processorMode: ProcessorMode): ARMInstruction =
    new ConditionalARMInstruction(condition) {
      override def encodeWord()(implicit page: MemoryPage) =
        (super.encodeWord() | 0x0120f000 | (destination.registerCode << 22 | ((fields.toBitMask)(0).toInt) | (source.registerCode)))

      override val toString = s"${mnemonic}${condition.mnemonicExtension} ${destination.toString}_${Fields.fieldsToString(fields)}, ${source.toString}"
    }

  // Immediate and shift
  def apply(source: Byte, rotate: Byte, destination: StatusRegister, fields: Fields.ValueSet, condition: Condition)(implicit processorMode: ProcessorMode): ARMInstruction = {
    assume(rotate >=0 && rotate <= 30 && (rotate % 2 == 0))
    new ConditionalARMInstruction(condition) {
      override def encodeWord()(implicit page: MemoryPage) =
        (super.encodeWord() | 0x0320f000 | (destination.registerCode << 22 | ((fields.toBitMask)(0).toInt) | (rotate << 7) | (source.toByte & 0xff)))

      override val toString = s"${mnemonic}${condition.mnemonicExtension} ${destination.toString}_${Fields.fieldsToString(fields)}, #${source.toString}, ${rotate}"
    }
  }
}