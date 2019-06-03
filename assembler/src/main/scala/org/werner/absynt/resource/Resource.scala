package org.werner.absynt.resource

import org.werner.absynt._

sealed abstract class Resource

sealed trait Labeled {
  def label: Label
  def resource: Resource
}

sealed abstract class Encodable extends Resource {
  def encodeByte: Seq[Byte]

  def size: Int
}

abstract class UnlabeledEncodable extends Encodable {
  final def label(newLabel: Label): Encodable with Labeled = new Encodable with Labeled {
    override def encodeByte: Seq[Byte] = UnlabeledEncodable.this.encodeByte

    override def size: Int = UnlabeledEncodable.this.size

    override def label: Label = newLabel

    override def resource: Resource = UnlabeledEncodable.this
  }
}

sealed abstract class DependentResource extends Resource {

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

  final def label(newLabel: Label): DependentResource with Labeled = new DependentResource with Labeled {

    override def label: Label = newLabel

    override val resource: Resource = UnlabeledDependentResource.this

    override def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable =
      UnlabeledDependentResource.this.unlabeledForDependencySize(dependencySize, offsetDirection).label(label)

    override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int =
      UnlabeledDependentResource.this.sizeForDependencySize(dependencySize, offsetDirection)

    override def possibleSizes: Set[Int] = UnlabeledDependentResource.this.possibleSizes

    override def dependencies(context: Application): (Seq[Resource], OffsetDirection) =
      UnlabeledDependentResource.this.dependencies(context)
  }

  final override def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable =
    unlabeledForDependencySize(dependencySize, offsetDirection)

  def unlabeledForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): UnlabeledEncodable
}
