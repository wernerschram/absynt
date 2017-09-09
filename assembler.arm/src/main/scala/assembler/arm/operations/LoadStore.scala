package assembler.arm.operations

import assembler.Label
import assembler.arm.operands.Condition.Condition
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operands.{ImmediateShiftValue, ShiftRegisterWithShift}
import assembler.sections.Section

import scala.language.implicitConversions

abstract sealed class LoadStoreAddressingType(pBit: Boolean, wBit: Boolean, val opcodeExtension: String) {
  val bitMask: Int = (if (pBit) 0x01000000 else 0) | (if (wBit) 0x00200000 else 0)

  final def formatParameters(baseRegister: GeneralRegister, offset: LoadStoreOffset): String =
    formatParameters(baseRegister, offset.toString)

  final def formatParameters(baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset): String =
    formatParameters(baseRegister, offset.toString)

  protected def formatParameters(baseRegister: GeneralRegister, offset: String): String
}

abstract sealed class LoadStoreAddressingTypeNormal private(pBit: Boolean, wBit: Boolean) extends LoadStoreAddressingType(pBit, wBit, "")

abstract sealed class LoadStoreAddressingTypeUser private(pBit: Boolean, wBit: Boolean) extends LoadStoreAddressingType(pBit, wBit, "t") {
  protected override def formatParameters(baseRegister: GeneralRegister, offset: String) =
    s"[$baseRegister], $offset"
}

object LoadStoreAddressingTypeNormal {

  object PostIndexedNormal extends LoadStoreAddressingTypeNormal(false, false) {
    protected override def formatParameters(baseRegister: GeneralRegister, offset: String) =
      s"[$baseRegister], $offset"
  }

  object OffsetNormal extends LoadStoreAddressingTypeNormal(true, false) {
    protected override def formatParameters(baseRegister: GeneralRegister, offset: String) =
      s"[$baseRegister, $offset]"
  }

  object PreIndexedNormal extends LoadStoreAddressingTypeNormal(true, true) {
    protected override def formatParameters(baseRegister: GeneralRegister, offset: String) =
      s"[$baseRegister, $offset]!"
  }

}

object LoadStoreAddressingTypeUser {

  object PostIndexedUser extends LoadStoreAddressingTypeUser(false, true)

}

abstract sealed class LoadStoreOffset private(val updateDirection: UpdateDirection.UpdateDirection) {
  def encode: Int
}

object LoadStoreOffset {
  implicit def apply(offset: Short, updateDirection: UpdateDirection.UpdateDirection) =
    new LoadStoreOffset(updateDirection) {
      override val encode: Int = 0x04000000 | offset | updateDirection.bitMask

      override def toString: String = s"#${updateDirection.sign}$offset"
    }

  val noOffset: LoadStoreOffset = apply(0.toShort)

  implicit def apply(offset: Short): LoadStoreOffset = if (offset >= 0)
    apply(offset, UpdateDirection.Increment)
  else
    apply((-offset).toShort, UpdateDirection.Decrement)

  implicit def apply(offsetRegister: GeneralRegister, updateDirection: UpdateDirection.UpdateDirection) =
    new LoadStoreOffset(updateDirection) {
      override val encode: Int = 0x06000000 | offsetRegister.registerCode | updateDirection.bitMask

      override def toString: String = s"${updateDirection.sign}$offsetRegister"
    }

  implicit def apply(offsetRegister: GeneralRegister): LoadStoreOffset = apply(offsetRegister, UpdateDirection.Increment)

  implicit def apply(offset: ShiftRegisterWithShift[ImmediateShiftValue], updateDirection: UpdateDirection.UpdateDirection) =
    new LoadStoreOffset(updateDirection) {
      override val encode: Int = 0x06000000 | offset.encode | updateDirection.bitMask

      override def toString: String = s"${updateDirection.sign}$offset"
    }

  implicit def apply(offset: ShiftRegisterWithShift[ImmediateShiftValue]): LoadStoreOffset = apply(offset, UpdateDirection.Increment)
}

abstract sealed class LoadStoreMiscelaneousOffset private(val updateDirection: UpdateDirection.UpdateDirection) {
  def encode: Int
}

object LoadStoreMiscelaneousOffset {
  def apply(offset: Byte, updateDirection: UpdateDirection.UpdateDirection) =
    new LoadStoreMiscelaneousOffset(updateDirection) {
      override val encode: Int = 0x00400090 | updateDirection.bitMask | ((offset & 0xf0) << 4) | (offset & 0x0f)

      override def toString: String = s"#${updateDirection.sign}$offset"
    }

  implicit def apply(offset: Byte): LoadStoreMiscelaneousOffset = if (offset >= 0)
    apply(offset, UpdateDirection.Increment)
  else
    apply((-offset).toByte, UpdateDirection.Decrement)

  def apply(offsetRegister: GeneralRegister, updateDirection: UpdateDirection.UpdateDirection) =
    new LoadStoreMiscelaneousOffset(updateDirection) {
      override val encode: Int = 0x00000090 | offsetRegister.registerCode | updateDirection.bitMask

      override def toString: String = s"${updateDirection.sign}$offsetRegister"
    }

  implicit def apply(offsetRegister: GeneralRegister): LoadStoreMiscelaneousOffset = apply(offsetRegister, UpdateDirection.Increment)
}

object UpdateDirection {

  sealed abstract class UpdateDirection(uBit: Boolean, val sign: String) {
    val bitMask: Int = if (uBit) 0x00800000 else 0
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

object LoadStoreMiscellaneousOperation {

  class LoadStoreMiscellaneousOperation(val bitMask: Int, val opcodeExtension: String)

  object StoreHalfWord extends LoadStoreMiscellaneousOperation(0x00000020, "h")
  object LoadDoubleWord extends LoadStoreMiscellaneousOperation(0x00000040, "d")
  object StoreDoubleWord extends LoadStoreMiscellaneousOperation(0x00000060, "d")
  object LoadUnsignedHalfWord extends LoadStoreMiscellaneousOperation(0x00100020, "h")
  object LoadSignedByte extends LoadStoreMiscellaneousOperation(0x00100040, "sb")
  object LoadSignedHalfWord extends LoadStoreMiscellaneousOperation(0x00100060, "sh")

}

class LoadStore(val label: Label, val opcode: String, val condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                offset: LoadStoreOffset, addressingType: LoadStoreAddressingType, operation: LoadStoreOperation.LoadStoreOperation)
  extends Conditional {
  override def encodeWord: Int =
    super.encodeWord |
      operation.bitMask | addressingType.bitMask |
      (baseRegister.registerCode << 16) | (register.registerCode << 12) | offset.encode

  override def toString =
    s"$labelPrefix$mnemonicString${operation.opcodeExtension}${addressingType.opcodeExtension} $register, ${addressingType.formatParameters(baseRegister, offset)}"

}

class LoadStoreMiscelaneous(val label: Label, val opcode: String, val condition: Condition, register: GeneralRegister,
                            baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingType,
                            operation: LoadStoreMiscellaneousOperation.LoadStoreMiscellaneousOperation)
  extends Conditional {

  override def encodeWord: Int =
    super.encodeWord |
      operation.bitMask | addressingType.bitMask |
      (baseRegister.registerCode << 16) | (register.registerCode << 12) | offset.encode

  override def toString =
    s"$labelPrefix$mnemonicString${operation.opcodeExtension}${addressingType.opcodeExtension} $register, ${addressingType.formatParameters(baseRegister, offset)}"
}
