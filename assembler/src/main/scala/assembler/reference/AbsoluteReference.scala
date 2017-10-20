package assembler.reference

import assembler._

sealed abstract case class AbsoluteReference[OffsetType<:Offset, AddressType<:Address[OffsetType]](target: Label, override val label: Label)
    extends Resource {

  def encodableForAddress(position: AddressType): Resource with Encodable

  def toInSectionState(application: Application[OffsetType, AddressType]): Resource = {
    val newMinimum: AddressType = application.getAbsoluteMinimumAddress(target)
    val newMaximum: AddressType = application.getAbsoluteMaximumAddress(target)
    if (newMinimum == newMaximum)
      encodableForAddress(newMinimum)
    else
      new AbsoluteReference[OffsetType, AddressType](target, label) {
        override def encodableForAddress(position: AddressType): Resource with Encodable =
          AbsoluteReference.this.encodableForAddress(position)

        override def minimumSize: Int = encodableForAddress(newMinimum).size

        override def maximumSize: Int = encodableForAddress(newMaximum).size
      }
  }
}

object AbsoluteReference {
  def apply[OffsetType<:Offset, AddressType<:Address[OffsetType]](target: Label, initialMinimumSize: Int, initialMaximumSize: Int, label: Label,
    encodableFactory: (AddressType)=>Resource with Encodable) =
    new AbsoluteReference[OffsetType, AddressType](target, label) {
      override def encodableForAddress(position: AddressType): Resource with Encodable = encodableFactory(position)

      override def minimumSize: Int = initialMinimumSize
      override def maximumSize: Int = initialMaximumSize

    }
}
