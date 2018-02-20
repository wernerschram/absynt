package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.operands._

sealed abstract case class FarPointer(segment: Short, offset: Seq[Byte])
  extends Operand {
  self: FarPointerSize =>

  override def toString =
    s"FAR 0x${segment.encodeLittleEndian.bigEndianHexString}:0x${offset.bigEndianHexString}"

  def encodeByte: Seq[Byte] = offset ++ segment.encodeLittleEndian
}

object FarPointer {
  def apply(segment: Short, offset: Short): FarPointer with FarPointerSize =
    new FarPointer(segment, offset.encodeLittleEndian) with FarDoubleWordSize

  def apply(segment: Short, offset: Int): FarPointer with FarPointerSize =
    new FarPointer(segment, offset.encodeLittleEndian) with FarWordSize
}
