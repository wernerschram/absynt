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
  private def forWord(value: Short): ImmediateValue with WordSize = new ImmediateValue(value.encodeLittleEndian) with WordSize {}
  private def forDoubleWord(value: Int): ImmediateValue with DoubleWordSize = new ImmediateValue(value.encodeLittleEndian) with DoubleWordSize {}
  private def forQuadWord(value: Long): ImmediateValue with QuadWordSize = new ImmediateValue(value.encodeLittleEndian) with QuadWordSize {}

  trait ForMode {
    def pointer(location: Long): ImmediateValue with WideSize
  }

  trait ForLegacy extends ForMode {
    implicit def byteImmedate(value: Byte): ImmediateValue with ByteSize = forByte(value)
    implicit def WordImmediate(value: Short): ImmediateValue with WordSize = forWord(value)

    override def pointer(location: Long): ImmediateValue with WideSize = forWord(location.toShort)
  }

  trait ForReal extends ForMode {
    implicit def byteImmedate(value: Byte): ImmediateValue with ByteSize = forByte(value)
    implicit def WordImmediate(value: Short): ImmediateValue with WordSize = forWord(value)
    implicit def doubleWordImmediate(value: Int): ImmediateValue with DoubleWordSize = forDoubleWord(value)

    override def pointer(location: Long): ImmediateValue with WideSize = forWord(location.toShort)
  }

  trait ForProtected extends ForMode {
    implicit def byteImmedate(value: Byte): ImmediateValue with ByteSize = forByte(value)
    implicit def WordImmediate(value: Short): ImmediateValue with WordSize = forWord(value)
    implicit def doubleWordImmediate(value: Int): ImmediateValue with DoubleWordSize = forDoubleWord(value)

    override def pointer(location: Long): ImmediateValue with WideSize = forDoubleWord(location.toInt)
  }

  trait ForLong extends ForMode {
    implicit def byteImmedate(value: Byte): ImmediateValue with ByteSize = forByte(value)
    implicit def WordImmediate(value: Short): ImmediateValue with WordSize = forWord(value)
    implicit def doubleWordImmediate(value: Int): ImmediateValue with DoubleWordSize = forDoubleWord(value)
    implicit def quadWordImmediate(value: Long): ImmediateValue with QuadWordSize = forQuadWord(value)

    override def pointer(location: Long): ImmediateValue with WideSize = forQuadWord(location)
  }
}