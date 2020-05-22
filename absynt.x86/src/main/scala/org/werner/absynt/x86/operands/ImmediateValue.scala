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

package org.werner.absynt.x86.operands

import org.werner.absynt.ListExtensions._
import org.werner.absynt.x86.operands.memoryaccess.{LongSIB, ProtectedSIB}

import scala.language.implicitConversions

sealed class ImmediateValue[S : Integral](val value: S)
  extends Operand {
  self: ValueSize =>

  val num: Integral[S] = implicitly[Integral[S]]
  import num._

  val encodedValue: Seq[Byte] = value.encodeLittleEndian

  val isPositive: Boolean = value.sign == one

  override def toString: String = value.toString
}

object ImmediateValue {

  def unapply[S:Integral](immediateValue: ImmediateValue[S]): Option[S] = Some(immediateValue.value)

  type ValueToByteImmediate = Byte => ImmediateValue[Byte] with ByteSize
  type ValueToWordImmediate = Short => ImmediateValue[Short] with WordSize
  type ValueToDoubleWordImmediate = Int => ImmediateValue[Int] with DoubleWordSize
  type ValueToQuadWordImmediate = Long => ImmediateValue[Long] with QuadWordSize



  val byteImmediate: ValueToByteImmediate = value => new ImmediateValue(value) with ByteSize {}
  val wordImmediate: ValueToWordImmediate = value => new ImmediateValue(value) with WordSize {}
  val doubleWordImmediate: ValueToDoubleWordImmediate = value => new ImmediateValue(value) with DoubleWordSize {}
  val quadWordImmediate: ValueToQuadWordImmediate = value => new ImmediateValue(value) with QuadWordSize {}

  trait I8086Implicits {
    implicit val byteImm: ValueToByteImmediate = byteImmediate
    implicit val wordImm: ValueToWordImmediate = wordImmediate
  }

  trait I386Implicits {
    implicit val doubleWordImm: ValueToDoubleWordImmediate = doubleWordImmediate

    case class ProtectedSIBImmediate(value: ImmediateValue[Int] with DoubleWordSize) extends ProtectedSIB {
      override val base: Option[GeneralPurposeRegister with ProtectedSIBBaseRegister with DoubleWordSize] = None
      override val index: Option[GeneralPurposeRegister with ProtectedSIBIndexRegister with DoubleWordSize] = None
      override val scale: Int = 1
      override val displacement: Option[ImmediateValue[Int] with DoubleWordSize] = Some(value)
      override val segment: SegmentRegister = Segment.Data
    }

    implicit def immediateValueIsProtectedSIBImmediate(value: ImmediateValue[Int] with DoubleWordSize): ProtectedSIBImmediate =
      ProtectedSIBImmediate(value)
  }

  trait X64Implicits {
    implicit val quadWordImm: ValueToQuadWordImmediate = quadWordImmediate

    case class LongSIBImmediate(value: ImmediateValue[Int] with DoubleWordSize) extends LongSIB {
      override val base: Option[GeneralPurposeRegister with LongSIBBaseRegister with QuadWordSize] = None
      override val index: Option[GeneralPurposeRegister with LongSIBIndexRegister with QuadWordSize] = None
      override val scale: Int = 1
      override val displacement: Option[ImmediateValue[Int] with DoubleWordSize] = Some(value)
      override val segment: SegmentRegister = Segment.Data
    }

    implicit def immediateValueIsLongSIBImmediate(value: ImmediateValue[Int] with DoubleWordSize): LongSIBImmediate =
      LongSIBImmediate(value)
  }
}