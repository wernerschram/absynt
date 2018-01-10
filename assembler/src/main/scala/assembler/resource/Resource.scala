package assembler.resource

import assembler._
import assembler.sections.Section
import EncodableConversion._

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

abstract class DependentResource(label: Label) extends Resource(label) {

  def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable

  def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int

  def possibleSizes: Set[Int]

  def dependencies(context: Application): (Seq[Resource], OffsetDirection)

  def applicationContextProperties(context: Application): (Seq[DependentResource], Int, OffsetDirection) = {
    val (resources, offsetType) = dependencies(context)

    val (totalDependent, totalIndependent) = resources
      .foldLeft((Seq.empty[DependentResource], 0)) {
        case ((dependent, independent), reference: DependentResource) => (dependent :+ reference, independent)
        case ((dependent, independent), encodable: Encodable) => (dependent, independent + encodable.size)
      }

    (totalDependent, totalIndependent, offsetType)
  }
}

case class AlignmentFiller(section: Section) extends DependentResource(Label.noLabel) {

  def dependencies(context: Application): (Seq[Resource], OffsetDirection) =
    (context.startFiller +: context.sectionDependencies(section), OffsetDirection.Absolute)

  override def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable =
    EncodedBytes(Seq.fill(sizeForDependencySize(dependencySize, offsetDirection))(0.toByte))(label)

  override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int = {
    val alignment = dependencySize % section.alignment
    if (alignment != 0)
      section.alignment - alignment
    else 0
  }

  override def possibleSizes: Set[Int] = (0 to section.alignment by 1).toSet

  override def toString: String = s"filler for ${section.name}"
}

abstract class RelativeReference(val target: Label, label: Label) extends DependentResource(label) {

  final def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable = {
    assume(offsetDirection.isInstanceOf[RelativeOffsetDirection])
    encodableForDistance(dependencySize, offsetDirection.asInstanceOf[RelativeOffsetDirection])
  }

  def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Encodable

  def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int

  override def dependencies(context: Application): (Seq[Resource], OffsetDirection) = {
    val section = context.sections.filter(s => s.content.contains(this)).head
      (section.intermediateResources(this), section.offsetDirection(this))
  }
}

abstract class AbsoluteReference(val target: Label, label: Label) extends DependentResource(label) {

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

  override def dependencies(context: Application): (Seq[Resource], OffsetDirection) = {
    val containingSection: Section = context.sections.filter(s => s.content.containsLabel(target)).head
    (
      context.startFiller +: (context.alignedSectionDependencies(containingSection) ++
      containingSection.precedingResources(target))
       ,OffsetDirection.Absolute
    )
  }
}

object EncodableConversion {
  implicit class Resources(resources: Seq[Resource]) {
    def encodables(dependentMap: Map[DependentResource, Encodable]): Seq[Encodable] = resources.map {
      case reference: DependentResource => dependentMap(reference)
      case encodable: Encodable => encodable
    }

    def dependentResources: Seq[DependentResource] = resources.collect{case r: DependentResource => r}

    def containsLabel(label: Label): Boolean = resources.exists(_.label == label)
  }

  implicit class Encodables(encodables: Seq[Encodable]) {
    def encodeByte: Seq[Byte] = encodables.flatMap { x => x.encodeByte }
  }
}
