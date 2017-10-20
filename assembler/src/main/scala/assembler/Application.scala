package assembler

import assembler.sections.{LastIteration, Section}

abstract class Application[OffsetType<:Offset, AddressType<:Address[OffsetType]] protected (
  val sections: List[Section[OffsetType]])
  (implicit addressFactory: AddressFactory[OffsetType, AddressType]) {

  lazy val encodableSections: List[Section[OffsetType] with LastIteration[OffsetType]] = sections.map(_.encodable(this))

  def getAbsoluteAddress(encodable: Resource): AddressType =
    encodableSections.filter(s=> s.contains(encodable))
      .map(s => addressFactory.add(memoryAddress(s), s.offset(encodable))).head

  def getAbsoluteAddress(label: Label): AddressType =
    encodableSections.filter(s => s.contains(label))
      .map(s => addressFactory.add(memoryAddress(s), s.offset(label))).head

  def memoryAddress(section: Section[OffsetType]): AddressType

  def getAbsoluteMinimumAddress(label: Label): AddressType =
    sections.filter(s => s.contains(label))
      .map(s => addressFactory.add(memoryAddress(s), s.minimumOffset(label))).head

  def getAbsoluteMaximumAddress(label: Label): AddressType =
    sections.filter(s => s.contains(label))
      .map(s => addressFactory.add(memoryAddress(s), s.maximumOffset(label))).head

  def encodeByte: List[Byte]
}
