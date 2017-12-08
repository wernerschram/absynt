package assembler.reference

import assembler._

sealed abstract case class AbsoluteReference(
  target: Label, override val label: Label)
    extends Reference {

  def encodeForDistance(distance: Int): Encodable

  final override def encodeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable = {
    assume(offsetDirection == OffsetDirection.Absolute)
    encodeForDistance(dependencySize)
  }

  def sizeForDistance(distance: Int): Int

  final override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int = {
    assume(offsetDirection == OffsetDirection.Absolute)
    sizeForDistance(dependencySize)
  }
}

object AbsoluteReference {
  def apply(target: Label, sizes: Set[Int], label: Label, encodableFactory: (Int) => Resource with Encodable): AbsoluteReference =
    new AbsoluteReference(target, label) {

      override def possibleSizes: Set[Int] = sizes

      override def encodeForDistance(distance: Int): Encodable = encodableFactory(distance)

      override def sizeForDistance(distance: Int): Int = encodableFactory(distance).size

    }
}
