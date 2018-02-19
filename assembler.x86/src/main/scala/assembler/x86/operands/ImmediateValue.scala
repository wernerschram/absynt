package assembler.x86.operands

import assembler.ListExtensions._

import scala.language.implicitConversions

sealed abstract class ImmediateValue(val value: Seq[Byte])
  extends Operand with ValueSize2 {

  val isPositive: Boolean = (value.last & 0x80.toByte) == 0

  override def toString: String = value.decimalString
}

object ImmediateValue {
  implicit def apply(value: Byte): ImmediateValue = new ImmediateValue(value.encodeLittleEndian) with ByteSize {}

  implicit def apply(value: Short): ImmediateValue = new ImmediateValue(value.encodeLittleEndian) with WordSize {}

  implicit def apply(value: Int): ImmediateValue = new ImmediateValue(value.encodeLittleEndian) with DoubleWordSize {}

  implicit def apply(value: Long): ImmediateValue = new ImmediateValue(value.encodeLittleEndian) with QuadWordSize {}
}