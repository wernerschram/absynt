package assembler.x86.operands

import assembler.ListExtensions._

import scala.language.implicitConversions

sealed class ImmediateValue(val value: Seq[Byte])
  extends Operand {
  self: ValueSize =>

  val isPositive: Boolean = (value.last & 0x80.toByte) == 0

  override def toString: String = value.decimalString
}

object ImmediateValue {
  implicit def apply(value: Byte): ImmediateValue with ByteSize =
    new ImmediateValue(value.encodeLittleEndian) with ByteSize {}

  implicit def apply(value: Short): ImmediateValue with WordSize =
    new ImmediateValue(value.encodeLittleEndian) with WordSize {}

  implicit def apply(value: Int): ImmediateValue with DoubleWordSize =
    new ImmediateValue(value.encodeLittleEndian) with DoubleWordSize {}

  implicit def apply(value: Long): ImmediateValue with QuadWordSize =
    new ImmediateValue(value.encodeLittleEndian) with QuadWordSize {}
}