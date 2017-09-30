package assembler

abstract class Address[OffsetType:Numeric] {
  def add(offset: OffsetType): Address[OffsetType]

  def +(that: OffsetType): Address[OffsetType] = this add that
}

