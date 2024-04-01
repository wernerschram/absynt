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

package org.werner.absynt

import org.werner.absynt.resource.{AbsoluteReference, UnlabeledEncodable, RelativeReference, Resource}

case class LinearRelativeTestEncodable(distance: Int, offsetDirection: RelativeOffsetDirection) extends UnlabeledEncodable {
  override def encodeByte: Seq[Byte] =
    offsetDirection match {
      case OffsetDirection.Forward => Seq.fill(size)(0xff.toByte)
      case OffsetDirection.Backward => Seq.fill(size)(0xbb.toByte)
      case OffsetDirection.Self => Seq.fill(size)(0x88.toByte)
    }

  override def size: Int =
    if distance < 10 then 1
    else if distance < 20 then 2
    else 3
}

case class LinearRelativeTestReference(override val target: Label) extends RelativeReference() {

  override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): UnlabeledEncodable =
    LinearRelativeTestEncodable(distance, offsetDirection)

  override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int =
    if dependencySize < 10 then 1
    else if dependencySize < 20 then 2
    else 3

  override def possibleSizes: Set[Int] = Set(1, 2, 3)
}


case class NonLinearRelativeTestEncodable(distance: Int, offsetDirection: RelativeOffsetDirection) extends UnlabeledEncodable {
  override def encodeByte: Seq[Byte] =
    offsetDirection match {
      case OffsetDirection.Forward => Seq.fill(size)(0xff.toByte)
      case OffsetDirection.Backward => Seq.fill(size)(0xbb.toByte)
      case OffsetDirection.Self => Seq.fill(size)(0x88.toByte)
    }

  override def size: Int =
    if distance < 10 then 1
    else if distance < 20 then 3
    else 2
}

case class NonLinearRelativeTestReference(override val target: Label) extends RelativeReference {
  override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): UnlabeledEncodable =
    NonLinearRelativeTestEncodable(distance, offsetDirection)

  override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int =
    if dependencySize < 10 then 1
    else if dependencySize < 20 then 3
    else 2

  override def possibleSizes: Set[Int] = Set(1, 2, 3)
}

case class AbsoluteTestEncodable(distance: Int) extends UnlabeledEncodable {
  override def encodeByte: Seq[Byte] = Seq.fill(size)(0xaa.toByte)

  override def size: Int =
    if distance < 10 then 1
    else if distance < 20 then 2
    else 3
}

case class AbsoluteTestReference(override val target: Label) extends AbsoluteReference(target) {
  override def encodableForDistance(distance: Int): UnlabeledEncodable = AbsoluteTestEncodable(distance)

  override def sizeForDistance(distance: Int): Int = encodableForDistance(distance).size

  override def possibleSizes: Set[Int] = Set(1, 2, 3)
}

object TestEncodable {
  def linearReferenceWithTarget: (LinearRelativeTestReference, Resource) = {
    val targetLabel = Label.unique
    val reference = LinearRelativeTestReference(targetLabel)
    val targetResource = EncodedBytes(Seq(0x00.toByte)).label(targetLabel)
    (reference, targetResource)
  }

  def nonLinearReferenceWithTarget: (NonLinearRelativeTestReference, Resource) = {
    val targetLabel = Label.unique
    val reference = NonLinearRelativeTestReference(targetLabel)
    val targetResource = EncodedBytes(Seq(0x00.toByte)).label(targetLabel)
    (reference, targetResource)
  }

  def absoluteReferenceWithTarget: (AbsoluteReference, Resource) = {
    val targetLabel = Label.unique
    val reference = AbsoluteTestReference(targetLabel)
    val targetResource = EncodedBytes(Seq(0x00.toByte)).label(targetLabel)
    (reference, targetResource)
  }
}
