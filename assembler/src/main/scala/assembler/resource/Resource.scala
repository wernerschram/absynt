package assembler.resource

import assembler._
import assembler.sections.Section

sealed abstract class Resource(val label: Label) {

  final val labelPrefix: String =
    label match {
      case _: NoLabel => ""
      case _ => s"$label: "
    }

  override def toString: String = labelPrefix
}

abstract class Encodable(label: Label) extends Resource(label) {
  def encodeByte: Seq[Byte]

  def size: Int
}

sealed abstract class DependentResource(label: Label) extends Resource(label) {

  def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable

  def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int

  def possibleSizes: Set[Int]
}

sealed abstract class Reference(val target: Label, label: Label) extends DependentResource(label)

abstract class AlignmentFiller(label: Label) extends DependentResource(label) {
  def section: Section
}

abstract class RelativeReference(target: Label, label: Label) extends Reference(target, label) {

  final def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable = {
    assume(offsetDirection.isInstanceOf[RelativeOffsetDirection])
    encodableForDistance(dependencySize, offsetDirection.asInstanceOf[RelativeOffsetDirection])
  }

  def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Encodable

  def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int
}

abstract class AbsoluteReference(target: Label, label: Label) extends Reference(target, label) {

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

