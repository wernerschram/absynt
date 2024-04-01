/*
 * Copyright 2019 Werner Schram
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

package org.werner.absynt.resource

import org.werner.absynt._

sealed abstract class Resource

sealed trait Labeled {
  self: Resource =>
  def label: Label
  def resource: Resource
}

sealed abstract class Encodable extends Resource {
  def encodeByte: Seq[Byte]

  def size: Int
}

sealed abstract class LabeledEncodable extends Encodable with Labeled {}

abstract class UnlabeledEncodable extends Encodable {
  final def label(newLabel: Label): Encodable & Labeled = new LabeledEncodable {
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

sealed abstract class LabeledDependentResource extends DependentResource with Labeled {}
abstract class UnlabeledDependentResource extends DependentResource {

  final def label(newLabel: Label): DependentResource & Labeled = new LabeledDependentResource {
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
