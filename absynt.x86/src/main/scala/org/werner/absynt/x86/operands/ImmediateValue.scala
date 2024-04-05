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

import org.werner.absynt.ListExtensions.*

import scala.runtime.stdLibPatches.Predef.summon

sealed class ImmediateValue[S : Integral](val value: S)
  extends Operand {
  self: ValueSize =>

  val num: Integral[S] = summon[Integral[S]]
  import num._

  val encodedValue: Seq[Byte] = value.encodeLittleEndian

  val isPositive: Boolean = value.sign == one

  override def toString: String = value.toString
}

object ImmediateValue {

  def unapply[S:Integral](immediateValue: ImmediateValue[S]): Option[S] = Some(immediateValue.value)

  type ValueToByteImmediate = Conversion[Byte, ImmediateValue[Byte] & ByteSize]
  type ValueToWordImmediate = Conversion[Short, ImmediateValue[Short] & WordSize]
  type ValueToDoubleWordImmediate = Conversion[Int, ImmediateValue[Int] & DoubleWordSize]
  type ValueToQuadWordImmediate = Conversion[Long, ImmediateValue[Long] & QuadWordSize]

  val byteImmediate: ValueToByteImmediate = value => new ImmediateValue(value) with ByteSize {}
  val wordImmediate: ValueToWordImmediate = value => new ImmediateValue(value) with WordSize {}
  val doubleWordImmediate: ValueToDoubleWordImmediate = value => new ImmediateValue(value) with DoubleWordSize {}
  val quadWordImmediate: ValueToQuadWordImmediate = value => new ImmediateValue(value) with QuadWordSize {}

  trait I8086Implicits {
    given ValueToByteImmediate = byteImmediate
    given ValueToWordImmediate = wordImmediate
  }

  trait I386Implicits {
    given ValueToDoubleWordImmediate = doubleWordImmediate
  }

  trait X64Implicits {
    given ValueToQuadWordImmediate = quadWordImmediate
  }
}