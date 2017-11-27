package assembler.reference

import assembler._

sealed abstract case class AbsoluteReference[OffsetType<:Offset, AddressType<:Address[OffsetType]](
  target: Label, override val label: Label)
    extends Reference {

  def encodeForDistance(distance: Int): Encodable

  override def encodeForDistance(distance: Int, offsetDirection: OffsetDirection): Encodable = {
    assume(offsetDirection == OffsetDirection.Absolute)
    encodeForDistance(distance)
  }

  def sizeForDistance(distance: Int): Int

  override def sizeForDistance(distance: Int, offsetDirection: OffsetDirection): Int = {
    assume(offsetDirection == OffsetDirection.Absolute)
    sizeForDistance(distance)
  }
}

object AbsoluteReference {
  def apply[OffsetType<:Offset, AddressType<:Address[OffsetType]](target: Label, initialEstimatedSize: Estimate[Int],
    sizes: Seq[Int], label: Label, encodableFactory: (AddressType)=>Resource with Encodable, encodableFactor: (Int) => Resource with Encodable) =
    new AbsoluteReference[OffsetType, AddressType](target, label) {

      override def estimateSize: Estimate[Int] = initialEstimatedSize

      override def possibleSizes: Seq[Int] = sizes

      override def encodeForDistance(distance: Int): Encodable = encodableFactor(distance)

      override def sizeForDistance(distance: Int): Int = encodableFactor(distance).size

    }
}
