package assembler

sealed trait OffsetDirection

sealed trait RelativeOffsetDirection extends OffsetDirection

object OffsetDirection {
  case object Absolute extends OffsetDirection
  case object Forward extends RelativeOffsetDirection
  case object Backward extends RelativeOffsetDirection
  case object Self extends RelativeOffsetDirection
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
  def positionalOffset(offsetValue: Long)(offsetDirection: RelativeOffsetDirection)(instructionSize: Int): OffsetType with RelativeOffset
  def add(offset: OffsetType, that: OffsetType with RelativeOffset): OffsetType with RelativeOffset
  def add(offset: OffsetType, that: Long): OffsetType with RelativeOffset
}
