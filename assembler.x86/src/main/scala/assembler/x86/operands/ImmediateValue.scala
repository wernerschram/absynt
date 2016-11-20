package assembler.x86.operands

import scala.language.implicitConversions

import assembler.ListExtensions._
import assembler.x86.ParameterPosition
import assembler.x86.RexExtendedRequirement

final class ImmediateValue(val value: List[Byte]) extends Operand with FixedSizeOperand {
  assume(List(1, 2, 4, 8).contains(value.length))

  override val operandByteSize: Int = value.length

  override def getRexRequirements(position: ParameterPosition): List[RexExtendedRequirement] =
    Nil

  val isPositive = (value.last & 0x80.toByte) == 0

  override def toString() = value.decimalString()
}

object ImmediateValue {
  implicit def byteToImmediate(value: Byte): ImmediateValue = new ImmediateValue(value.encodeLittleEndian)
  implicit def shortToImmediate(value: Short): ImmediateValue = new ImmediateValue(value.encodeLittleEndian)
  implicit def intToImmediate(value: Int): ImmediateValue = new ImmediateValue(value.encodeLittleEndian)
  implicit def longToImmediate(value: Long): ImmediateValue = new ImmediateValue(value.encodeLittleEndian)
}