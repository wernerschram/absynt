package assembler.arm.opcodes

import scala.language.implicitConversions

import assembler.arm.instructions.ARMInstruction
import assembler.arm.instructions.ConditionalARMInstruction
import assembler.arm.operands.Condition.Condition
import assembler.arm.operands.ImmediateShiftValue
import assembler.arm.operands.ShiftRegisterWithShift
import assembler.arm.operands.registers.GeneralRegister
import assembler.memory.MemoryPage

abstract sealed class LoadStoreAddressingType(pBit: Boolean, wBit: Boolean, val opcodeExtension: String) {
  protected def formatParameters(baseRegister: GeneralRegister, offset: String): String

  final def formatParameters(baseRegister: GeneralRegister, offset: LoadStoreOffset): String = formatParameters(baseRegister, offset.toString)

  final def formatMiscelaneousParameters(baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset) = formatParameters(baseRegister, offset.toString)

  val bitMask = (if (pBit) 0x01000000 else 0) | (if (wBit) 0x00200000 else 0)
}

abstract sealed class LoadStoreAddressingTypeNormal private (pBit: Boolean, wBit: Boolean) extends LoadStoreAddressingType(pBit, wBit, "")

abstract sealed class LoadStoreAddressingTypeUser private (pBit: Boolean, wBit: Boolean) extends LoadStoreAddressingType(pBit, wBit, "t") {
  protected override def formatParameters(baseRegister: GeneralRegister, offset: String) =
    s"[${baseRegister}], ${offset}"
}

object LoadStoreAddressingTypeNormal {
  object PostIndexedNormal extends LoadStoreAddressingTypeNormal(false, false) {
    protected override def formatParameters(baseRegister: GeneralRegister, offset: String) =
      s"[${baseRegister}], ${offset}"
  }
  object OffsetNormal extends LoadStoreAddressingTypeNormal(true, false) {
    protected override def formatParameters(baseRegister: GeneralRegister, offset: String) =
      s"[${baseRegister}, ${offset}]"
  }
  object PreIndexedNormal extends LoadStoreAddressingTypeNormal(true, true) {
    protected override def formatParameters(baseRegister: GeneralRegister, offset: String) =
      s"[${baseRegister}, ${offset}]!"
  }
}

object LoadStoreAddressingTypeUser {
  object PostIndexedUser extends LoadStoreAddressingTypeUser(false, true)
}

abstract sealed class LoadStoreOffset private (val updateDirection: UpdateDirection.UpdateDirection) {
  def encode: Int
}

object LoadStoreOffset {
  implicit def apply(offset: Short, updateDirection: UpdateDirection.UpdateDirection) = new LoadStoreOffset(updateDirection) {
    override val encode = 0x04000000 | offset | updateDirection.bitMask
    override val toString: String = s"#${updateDirection.sign}${offset}"
  }

  implicit def apply(offset: Short): LoadStoreOffset = {
    if (offset >= 0)
      apply(offset, UpdateDirection.Increment)
    else 
      apply((-offset).toShort, UpdateDirection.Decrement)
  }

  implicit def apply(offsetRegister: GeneralRegister, updateDirection: UpdateDirection.UpdateDirection) = new LoadStoreOffset(updateDirection) {
    override val encode = 0x06000000 | offsetRegister.registerCode | updateDirection.bitMask
    override val toString: String = s"${updateDirection.sign}${offsetRegister}"
  }

  implicit def apply(offsetRegister: GeneralRegister): LoadStoreOffset = apply(offsetRegister, UpdateDirection.Increment)

  implicit def apply(offset: ShiftRegisterWithShift[ImmediateShiftValue], updateDirection: UpdateDirection.UpdateDirection) = new LoadStoreOffset(updateDirection) {
    override val encode = 0x06000000 | offset.encode | updateDirection.bitMask
    override val toString: String = s"#${updateDirection.sign}${offset}"
  }

  implicit def apply(offset: ShiftRegisterWithShift[ImmediateShiftValue]): LoadStoreOffset = apply(offset, UpdateDirection.Increment)
}

abstract sealed class LoadStoreMiscelaneousOffset private (val updateDirection: UpdateDirection.UpdateDirection) {
  def encode: Int
}

object LoadStoreMiscelaneousOffset {
  implicit def apply(offset: Byte, updateDirection: UpdateDirection.UpdateDirection) = new LoadStoreMiscelaneousOffset(updateDirection) {
    override val encode = 0x00400090 | updateDirection.bitMask | ((offset & 0xf0) << 4) | (offset & 0x0f)
    override val toString: String = s"#${updateDirection.sign}${offset}"
  }

  implicit def apply(offset: Byte): LoadStoreMiscelaneousOffset = apply(offset, UpdateDirection.Increment)

  implicit def apply(offsetRegister: GeneralRegister, updateDirection: UpdateDirection.UpdateDirection) = new LoadStoreMiscelaneousOffset(updateDirection) {
    override val encode = 0x00000090 | offsetRegister.registerCode | updateDirection.bitMask
    override val toString: String = s"${updateDirection.sign}${offsetRegister}"
  }

  implicit def apply(offsetRegister: GeneralRegister): LoadStoreMiscelaneousOffset = apply(offsetRegister, UpdateDirection.Increment)
}


object UpdateDirection {
  sealed abstract class UpdateDirection(uBit: Boolean, val sign: String) {
    val bitMask = (if (uBit) 0x00800000 else 0)
  }

  object Increment extends UpdateDirection(true, "")
  object Decrement extends UpdateDirection(false, "-")
}

object LoadStoreOperation {
  abstract sealed class LoadStoreOperation(val bitMask: Int, val opcodeExtension: String)
  
  object StoreWord extends LoadStoreOperation(0x00000000, "")
  object LoadWord extends LoadStoreOperation(0x00100000, "")
  object StoreByte extends LoadStoreOperation(0x00400000, "b")
  object LoadByte extends LoadStoreOperation(0x00500000, "b")
}

object LoadStoreMiscelaneousOperation {
  class LoadStoreMiscelaneousOperation(val bitMask: Int, val opcodeExtension: String)
  
  object StoreHalfWord extends LoadStoreMiscelaneousOperation(0x00000020, "h")
  object LoadDoubleWord extends LoadStoreMiscelaneousOperation(0x00000040, "d")
  object StoreDoubleWord extends LoadStoreMiscelaneousOperation(0x00000060, "d")
  object LoadUnsignedHalfWord extends LoadStoreMiscelaneousOperation(0x00100020, "h")
  object LoadSignedByte extends LoadStoreMiscelaneousOperation(0x00100040, "sb")
  object LoadSignedHalfWord extends LoadStoreMiscelaneousOperation(0x00100060, "sh")
}

class LoadStore(operation: LoadStoreOperation.LoadStoreOperation)(implicit mnemonic: String)
    extends Opcode(mnemonic) {

  def apply(
    condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset,
    addressingType: LoadStoreAddressingType): ARMInstruction = {

    new ConditionalARMInstruction(condition) {
      override def encodeWord()(implicit page: MemoryPage) =
        (super.encodeWord() |
          operation.bitMask | addressingType.bitMask |
          (baseRegister.registerCode << 16) | (register.registerCode << 12) | offset.encode)

      override val toString =
        s"${mnemonic}${operation.opcodeExtension}${addressingType.opcodeExtension} ${register}, ${addressingType.formatParameters(baseRegister, offset)}"

    }
  }
}

class LoadStoreMiscelaneous(operation: LoadStoreMiscelaneousOperation.LoadStoreMiscelaneousOperation)(implicit mnemonic: String)
    extends Opcode(mnemonic) {

  def apply(
    condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset,
    addressingType: LoadStoreAddressingType): ARMInstruction = {

    new ConditionalARMInstruction(condition) {
      override def encodeWord()(implicit page: MemoryPage) =
        (super.encodeWord() |
          operation.bitMask | addressingType.bitMask |
          (baseRegister.registerCode << 16) | (register.registerCode << 12) | offset.encode)

      override val toString =
        s"${mnemonic}${operation.opcodeExtension}${addressingType.opcodeExtension} ${register}, ${addressingType.formatMiscelaneousParameters(baseRegister, offset)}"
    }
  }
}