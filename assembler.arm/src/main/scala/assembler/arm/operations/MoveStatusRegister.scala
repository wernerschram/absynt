package assembler.arm.operations

import scala.language.implicitConversions

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition.Condition
import assembler.arm.operands.registers._
import assembler.memory.MemoryPage
import assembler.arm.operands.RightRotateImmediate

class MoveFromStatusRegister(override val opcode: String, source: StatusRegister, destination: GeneralRegister, override val condition: Condition)
    extends Conditional {
  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | 0x010f0000 | (source.registerCode << 22) | (destination.registerCode << 12))

  override def toString = s"${super.toString()} ${destination.toString}, ${source.toString}"
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

class MoveToStatusRegister private(override val opcode: String, destination: StatusRegister, fields: Fields.ValueSet, override val condition: Condition, val sourceString: String, val sourceValue: Int)
    extends Conditional {

  def this(opcode: String, source: GeneralRegister, destination: StatusRegister, fields: Fields.ValueSet, condition: Condition) =
    this(opcode, destination, fields, condition, source.toString, source.registerCode)

  def this(opcode: String, source: RightRotateImmediate, destination: StatusRegister, fields: Fields.ValueSet, condition: Condition) =
    this(opcode, destination, fields, condition, source.toString, source.encode)

  override def encodeWord()(implicit page: MemoryPage) =
    super.encodeWord() | 0x0120f000 | (destination.registerCode << 22 | ((fields.toBitMask)(0).toInt) | sourceValue)

  override def toString = s"${super.toString()} ${destination.toString}_${Fields.fieldsToString(fields)}, ${sourceString}"
}
