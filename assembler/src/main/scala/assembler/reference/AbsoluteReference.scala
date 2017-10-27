package assembler.reference

import assembler._

sealed abstract case class AbsoluteReference[OffsetType<:Offset, AddressType<:Address[OffsetType]](target: Label, override val label: Label)
    extends Resource {

  def encodableForAddress(position: AddressType): Resource with Encodable

  def toInSectionState(application: Application[OffsetType, AddressType]): Resource = {
    val newEstimate: Estimate[AddressType] = application.estimateAbsoluteAddress(target)
    newEstimate match {
      case actual: Actual[AddressType] => encodableForAddress(actual.value)
      case bounded: Bounded[AddressType] => new AbsoluteReference[OffsetType, AddressType](target, label) {
        override def encodableForAddress(position: AddressType): Resource with Encodable =
          AbsoluteReference.this.encodableForAddress(position)

        override def estimateSize: Estimate[Int] = bounded.map(encodableForAddress(_).size)
      }
      case _ => throw new AssertionError()
    }
  }
}

object AbsoluteReference {
  def apply[OffsetType<:Offset, AddressType<:Address[OffsetType]](target: Label, initialMinimumSize: Int, initialMaximumSize: Int, label: Label,
    encodableFactory: (AddressType)=>Resource with Encodable) =
    new AbsoluteReference[OffsetType, AddressType](target, label) {
      override def encodableForAddress(position: AddressType): Resource with Encodable = encodableFactory(position)

      override def estimateSize: Estimate[Int] = Estimate(initialMinimumSize, initialMaximumSize)
    }
}
