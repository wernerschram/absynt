package assembler

abstract class Address[OffsetType <: Offset] {
  def toLong: Long // FIXME: toLong is used for encoding addresses in Elf files. This should be replaced by some function in ProcessorClass
}

sealed trait OffsetDirection

object OffsetDirection {
  case object Forward extends OffsetDirection
  case object Backward extends OffsetDirection
  case object Absolute extends OffsetDirection
  case object Self extends OffsetDirection
}

class Offset

trait RelativeOffset {
  self: Offset =>
}

trait AbsoluteOffset {
  self: Offset =>
}

trait OffsetFactory[OffsetType <: Offset] {
  def offset(offsetValue: Long): OffsetType with RelativeOffset
  def positionalOffset(offsetValue: Long)(offsetDirection: OffsetDirection)(instructionSize: Int): OffsetType with RelativeOffset
  def add(offset: OffsetType, that: OffsetType with RelativeOffset): OffsetType with RelativeOffset
  def add(offset: OffsetType, that: Long): OffsetType with RelativeOffset
}
