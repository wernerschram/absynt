package assembler.resource

import assembler._
import assembler.sections.Section

sealed trait Resource {
  def label: Label

  lazy val labelPrefix: String =
    label match {
      case _: NoLabel => ""
      case _ => s"$label: "
    }

  override def toString: String = labelPrefix
}

trait Encodable extends Resource {
  def encodeByte: Seq[Byte]

  def size: Int
}

sealed trait DependentResource extends Resource {

  def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable

  def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int

  def possibleSizes: Set[Int]
}

sealed trait Reference extends DependentResource {
  def target: Label
}

trait AlignmentFiller extends DependentResource {
  def section: Section

  override def label: Label = Label.noLabel
}

trait RelativeReference
    extends Reference {

  final def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable =
    offsetDirection match {
      case direction: RelativeOffsetDirection => encodableForDistance(dependencySize, direction)
      case _ => throw new AssertionError()
    }

  def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Encodable

  def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int
}
sealed abstract case class AbsoluteReference(
  target: Label, override val label: Label)
    extends Reference {

  def encodableForDistance(distance: Int): Encodable

  final override def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable = {
    assume(offsetDirection == OffsetDirection.Absolute)
    encodableForDistance(dependencySize)
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

      override def encodableForDistance(distance: Int): Encodable = encodableFactory(distance)

      override def sizeForDistance(distance: Int): Int = encodableFactory(distance).size

    }
}
