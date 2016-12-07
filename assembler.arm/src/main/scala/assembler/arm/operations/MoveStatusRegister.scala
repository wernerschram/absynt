package assembler.arm.operations

import scala.language.implicitConversions

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition.Condition
import assembler.arm.operands.registers._
import assembler.memory.MemoryPage
import assembler.arm.operands.RightRotateImmediate

class MoveFromStatusRegister()(implicit mnemonic: String)
    extends Operation(mnemonic) {

  def apply(source: StatusRegister, destination: GeneralRegister, conditionValue: Condition)(implicit processorMode: ProcessorMode): ARMOperation =
    new Conditional {
      val condition = conditionValue
      override val opcode = MoveFromStatusRegister.this.mnemonic

      override def encodeWord()(implicit page: MemoryPage) =
        (super.encodeWord() | 0x010f0000 | (source.registerCode << 22) | (destination.registerCode << 12))

      override def toString = s"${super.toString()} ${destination.toString}, ${source.toString}"
    }
}

object Fields extends Enumeration {
  type Fields = Value

  val control = Value(16, "c")
  val extension = Value(17, "x")
  val status = Value(18, "s")
  val flags = Value(19, "f")

  implicit def fieldsToString(set: ValueSet): String = {
    set.foldRight("")((a, b) => a + b).reverse
  }
}

class MoveToStatusRegister()(implicit mnemonic: String)
    extends Operation(mnemonic) {

  def apply(source: GeneralRegister, destination: StatusRegister, fields: Fields.ValueSet, conditionValue: Condition)(implicit processorMode: ProcessorMode): ARMOperation =
    new Conditional {
      val condition = conditionValue
      override val opcode = MoveToStatusRegister.this.mnemonic

      override def encodeWord()(implicit page: MemoryPage) =
        (super.encodeWord() | 0x0120f000 | (destination.registerCode << 22 | ((fields.toBitMask)(0).toInt) | (source.registerCode)))

      override def toString = s"${super.toString()}${condition.mnemonicExtension} ${destination.toString}_${Fields.fieldsToString(fields)}, ${source.toString}"
    }

  def apply(source: RightRotateImmediate, destination: StatusRegister, fields: Fields.ValueSet, conditionValue: Condition)(implicit processorMode: ProcessorMode): ARMOperation = {
    new Conditional {
      val condition = conditionValue
      override val opcode = MoveToStatusRegister.this.mnemonic

      override def encodeWord()(implicit page: MemoryPage) =
        (super.encodeWord() | 0x0120f000 | (destination.registerCode << 22) | ((fields.toBitMask)(0).toInt) | source.encode)

      override def toString = s"${super.toString()}${condition.mnemonicExtension} ${destination.toString}_${Fields.fieldsToString(fields)}, ${source.toString}"
    }
  }
}