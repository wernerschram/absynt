package assembler.x86.operands

import assembler.ListExtensions._

import scala.language.implicitConversions

sealed abstract class ImmediateValue(val value: Seq[Byte], override val operandByteSize: OperandSize)
  extends Operand with FixedSizeOperand {

  val isPositive: Boolean = (value.last & 0x80.toByte) == 0

  override def toString: String = value.decimalString
}

object ImmediateValue {
  implicit def byteToImmediate(value: Byte): ImmediateValue = new ImmediateValue(value.encodeLittleEndian, ValueSize.Byte) {}

  implicit def shortToImmediate(value: Short): ImmediateValue = new ImmediateValue(value.encodeLittleEndian, ValueSize.Word) {}

  implicit def intToImmediate(value: Int): ImmediateValue = new ImmediateValue(value.encodeLittleEndian, ValueSize.DoubleWord) {}

  implicit def longToImmediate(value: Long): ImmediateValue = new ImmediateValue(value.encodeLittleEndian, ValueSize.QuadWord) {}
}