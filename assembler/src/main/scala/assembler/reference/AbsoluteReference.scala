package assembler.reference

import assembler._

sealed abstract case class AbsoluteReference[OffsetType<:Offset, AddressType<:Address[OffsetType]](
  target: Label, override val label: Label)
    extends Reference {

  def encodableForAddress(position: AddressType): Resource with Encodable

  def bind(application: Application[OffsetType, AddressType]): Resource = {
    val newEstimate: Estimate[AddressType] = application.estimateAbsoluteAddress(target)
    newEstimate match {
      case actual: Actual[AddressType] => encodableForAddress(actual.value)
      case bounded: Bounded[AddressType] => new AbsoluteReference[OffsetType, AddressType](target, label) {
        override def encodableForAddress(position: AddressType): Resource with Encodable =
          AbsoluteReference.this.encodableForAddress(position)

        override def estimateSize: Estimate[Int] = bounded.map(encodableForAddress(_).size)

        override def possibleSizes: Seq[Int] =
          AbsoluteReference.this.possibleSizes

        override def encodeForDistance(distance: Int): Resource with Encodable =
          AbsoluteReference.this.encodeForDistance(distance)

        override def sizeForDistance(distance: Int): Int =
          AbsoluteReference.this.sizeForDistance(distance)
      }
      case _ => throw new AssertionError()
    }
  }
}

object AbsoluteReference {
  def apply[OffsetType<:Offset, AddressType<:Address[OffsetType]](target: Label, initialEstimatedSize: Estimate[Int],
    sizes: Seq[Int], label: Label, encodableFactory: (AddressType)=>Resource with Encodable, encodableFactor: (Int) => Resource with Encodable) =
    new AbsoluteReference[OffsetType, AddressType](target, label) {
      override def encodableForAddress(position: AddressType): Resource with Encodable = encodableFactory(position)

      override def estimateSize: Estimate[Int] = initialEstimatedSize

      override def possibleSizes: Seq[Int] = sizes

      override def encodeForDistance(distance: Int): Encodable = encodableFactor(distance)

      override def sizeForDistance(distance: Int): Int = encodableFactor(distance).size

    }
}
