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

  def forByte(value: Byte): ImmediateValue with ByteSize = new ImmediateValue(value.encodeLittleEndian) with ByteSize {}
  def forWord(value: Short): ImmediateValue with WordSize = new ImmediateValue(value.encodeLittleEndian) with WordSize {}
  def forDoubleWord(value: Int): ImmediateValue with DoubleWordSize = new ImmediateValue(value.encodeLittleEndian) with DoubleWordSize {}
  def forQuadWord(value: Long): ImmediateValue with QuadWordSize = new ImmediateValue(value.encodeLittleEndian) with QuadWordSize {}

  trait I8086Implicits {
    implicit def byteImmedate(value: Byte): ImmediateValue with ByteSize = forByte(value)
    implicit def wordImmediate(value: Short): ImmediateValue with WordSize = forWord(value)
  }

  trait I386Implicits {
    implicit def doubleWordImmediate(value: Int): ImmediateValue with DoubleWordSize = forDoubleWord(value)
  }

  trait X64Implicits {
    implicit def quadWordImmediate(value: Long): ImmediateValue with QuadWordSize = forQuadWord(value)
  }
}