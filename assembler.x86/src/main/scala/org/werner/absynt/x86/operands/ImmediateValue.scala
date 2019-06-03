package org.werner.absynt.x86.operands

import org.werner.absynt.ListExtensions._

import scala.language.implicitConversions

sealed class ImmediateValue(val value: Seq[Byte])
  extends Operand {
  self: ValueSize =>

  val isPositive: Boolean = (value.last & 0x80.toByte) == 0

  override def toString: String = value.decimalString
}

object ImmediateValue {

  type ValueToByteImmediate = Byte => ImmediateValue with ByteSize
  type ValueToWordImmediate = Short => ImmediateValue with WordSize
  type ValueToDoubleWordImmediate = Int => ImmediateValue with DoubleWordSize
  type ValueToQuadWordImmediate = Long => ImmediateValue with QuadWordSize

  trait I8086Implicits {
    implicit val byteImm: ValueToByteImmediate = value => new ImmediateValue(value.encodeLittleEndian) with ByteSize {}
    implicit val wordImm: ValueToWordImmediate = value => new ImmediateValue(value.encodeLittleEndian) with WordSize {}
  }

  trait I386Implicits {
    implicit val doubleWordImm: ValueToDoubleWordImmediate = value => new ImmediateValue(value.encodeLittleEndian) with DoubleWordSize {}
  }

  trait X64Implicits {
    implicit val quadWordImm: ValueToQuadWordImmediate = value => new ImmediateValue(value.encodeLittleEndian) with QuadWordSize {}
  }
}