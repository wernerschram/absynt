package assembler

abstract class Address[OffsetType <: Offset] {
  def add(offset: OffsetType): Address[OffsetType]

  def +(that: OffsetType): Address[OffsetType] = this add that
}

trait Offset {

}
