package assembler

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

trait Reference extends DependentResource {
  def target: Label
}

trait AlignmentFiller extends DependentResource {
  def section: Section

  override def label: Label = Label.noLabel
}

