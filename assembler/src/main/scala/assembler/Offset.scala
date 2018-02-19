package assembler

sealed trait OffsetDirection

sealed trait RelativeOffsetDirection extends OffsetDirection

object OffsetDirection {
  case object Absolute extends OffsetDirection
  case object Forward extends RelativeOffsetDirection
  case object Backward extends RelativeOffsetDirection
  case object Self extends RelativeOffsetDirection
}


