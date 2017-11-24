package assembler

abstract class Address[OffsetType <: Offset] {
  def toLong: Long // FIXME: toLong is used for encoding addresses in Elf files. This should be replaced by some function in ProcessorClass
}

@deprecated("remove this when finished reimplementing References", "recent")
sealed trait OffsetDirectionOld

object OffsetDirectionOld {
  case object Forward extends OffsetDirectionOld
  case object Backward extends OffsetDirectionOld
  case object None extends OffsetDirectionOld
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
  def direction: OffsetDirectionOld
}

trait AbsoluteOffset {
  self: Offset =>
}

object RelativeOffset {
   implicit class EstimateListExtension[OffsetType <: Offset](l: Seq[Estimate[OffsetType with RelativeOffset]])(implicit offsetFactory: OffsetFactory[OffsetType]) {
    def estimateSum: Estimate[OffsetType with RelativeOffset] =
      l.reduceOption(Estimate.reduceInner[OffsetType with RelativeOffset](offsetFactory.add))
        .getOrElse(Actual[OffsetType with RelativeOffset](offsetFactory.offset(0)))
  }
}

trait AddressFactory[OffsetType <: Offset, AddressType<:Address[OffsetType]] {
  def zero: AddressType
  def add(address: AddressType, offset: OffsetType with RelativeOffset): AddressType
}

trait OffsetFactory[OffsetType <: Offset] {
  def offset(offsetValue: Long): OffsetType with RelativeOffset
  def positionalOffset(offsetValue: Long)(offsetDirection: OffsetDirectionOld)(instructionSize: Int): OffsetType with RelativeOffset
  def add(offset: OffsetType, that: OffsetType with RelativeOffset): OffsetType with RelativeOffset
  def add(offset: OffsetType, that: Long): OffsetType with RelativeOffset
}
