package assembler

abstract class Address[OffsetType <: Offset] {
  def toLong: Long // FIXME: toLong is used for encoding addresses in Elf files. This should be replaced by some function in ProcessorClass
}


sealed trait OffsetDirection

object OffsetDirection {
  case object Forward extends OffsetDirection
  case object Backward extends OffsetDirection
  case object None extends OffsetDirection
}

trait Offset {
  def direction: OffsetDirection
}

object Offset {
   implicit class EstimateListExtension[V<:Offset:OffsetFactory](l: Seq[Estimate[V]]) {
    def estimateSum: Estimate[V] =
      l.reduceOption(Estimate.reduceInner[V](implicitly[OffsetFactory[V]].add))
        .getOrElse(Actual[V](implicitly[OffsetFactory[V]].offset(0)))
  }
}

trait AddressFactory[OffsetType <: Offset, AddressType<:Address[OffsetType]] {
  def zero: AddressType
  def add(address: AddressType, offset: OffsetType): AddressType
}

trait OffsetFactory[OffsetType] {
  def offset(offsetValue: Long): OffsetType
  def offset(offsetDirection: OffsetDirection, offsetValue: Long): OffsetType = {
    assume(offsetValue>=0)
    offsetDirection match {
      case OffsetDirection.None => offset(0)
      case OffsetDirection.Forward => offset(offsetValue)
      case OffsetDirection.Backward => offset(-offsetValue)
    }
  }
  def add(offset: OffsetType, that: OffsetType): OffsetType
  def add(offset: OffsetType, that: Long): OffsetType
}

trait PositionalOffsetFactory[OffsetType] extends OffsetFactory[OffsetType] {
  def offset(instructionSize: Int, offsetDirection: OffsetDirection, offsetValue: Long): OffsetType
}
