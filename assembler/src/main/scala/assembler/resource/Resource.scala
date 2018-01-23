package assembler.resource

import assembler._
import assembler.sections.Section
import EncodableConversion._

sealed abstract class Resource {
}

sealed trait Labeled {
  def label: Label
  def resource: Resource
}

abstract class Encodable extends Resource {
  def encodeByte: Seq[Byte]

  def size: Int
}

abstract class UnlabeledEncodable extends Encodable {
  def label(label: Label): LabeledEncodable = new LabeledEncodable(this, label)
}

final class LabeledEncodable(encodable: UnlabeledEncodable, override val label: Label) extends Encodable with Labeled {
  override val resource: Resource = encodable

  override def encodeByte: Seq[Byte] = encodable.encodeByte

  override def size: Int = encodable.size
}

abstract class DependentResource extends Resource {

  def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable

  def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int

  def possibleSizes: Set[Int]

  def dependencies(context: Application): (Seq[Resource], OffsetDirection)

  final def applicationContextProperties(context: Application): (Seq[DependentResource], Int, OffsetDirection) = {
    val (resources, offsetType) = dependencies(context)

    val (totalDependent, totalIndependent) = resources
      .foldLeft((Seq.empty[DependentResource], 0)) {
        case ((dependent, independent), reference: DependentResource) => (dependent :+ reference, independent)
        case ((dependent, independent), encodable: Encodable) => (dependent, independent + encodable.size)
      }

    (totalDependent, totalIndependent, offsetType)
  }
}

abstract class UnlabeledDependentResource extends DependentResource {
  def label(label: Label): LabeledDependentResource = new LabeledDependentResource(this, label)

  final override def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable =
    unlabeledForDependencySize(dependencySize, offsetDirection)

  def unlabeledForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): UnlabeledEncodable
}


final class LabeledDependentResource(dependent: UnlabeledDependentResource, override val label: Label) extends DependentResource with Labeled {

  override val resource: Resource = dependent

  override def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable =
    dependent.unlabeledForDependencySize(dependencySize, offsetDirection).label(label)

  override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int =
    dependent.sizeForDependencySize(dependencySize, offsetDirection)

  override def possibleSizes: Set[Int] = dependent.possibleSizes

  override def dependencies(context: Application): (Seq[Resource], OffsetDirection) = dependent.dependencies(context)
}

case class AlignmentFiller(section: Section) extends UnlabeledDependentResource {

  def dependencies(context: Application): (Seq[Resource], OffsetDirection) =
    (context.startFiller +: context.sectionDependencies(section), OffsetDirection.Absolute)

  override def unlabeledForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): UnlabeledEncodable =
    EncodedBytes(Seq.fill(sizeForDependencySize(dependencySize, offsetDirection))(0.toByte))

  override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int = {
    val alignment = dependencySize % section.alignment
    if (alignment != 0)
      section.alignment - alignment
    else 0
  }

  override def possibleSizes: Set[Int] = (0 to section.alignment by 1).toSet

  override def toString: String = s"filler for ${section.name}"
}

abstract class RelativeReference(val target: Label) extends UnlabeledDependentResource {

  final def unlabeledForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): UnlabeledEncodable = {
    assume(offsetDirection.isInstanceOf[RelativeOffsetDirection])
    encodableForDistance(dependencySize, offsetDirection.asInstanceOf[RelativeOffsetDirection])
  }

  def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): UnlabeledEncodable

  def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int

  override def dependencies(context: Application): (Seq[Resource], OffsetDirection) = {
    val section = context.sections.filter(s => s.content.exists(r =>
        (r == this) || (r match {
          case l: Labeled => l.resource == this
          case _ => false
        }))).head

    (section.intermediateResources(this), section.offsetDirection(this))
  }
}

abstract class AbsoluteReference(val target: Label) extends UnlabeledDependentResource {

  def encodableForDistance(distance: Int): UnlabeledEncodable

  final override def unlabeledForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): UnlabeledEncodable = {
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

    def containsLabel(label: Label): Boolean =
      resources.collect{ case r: Labeled => r}.exists(_.label.matches(label))
  }

  implicit class Encodables(encodables: Seq[Encodable]) {
    def encodeByte: Seq[Byte] = encodables.flatMap { x => x.encodeByte }
  }
}
