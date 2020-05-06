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


sealed class ImmediateValue(val value: Seq[Byte])
  extends Operand {
  self: ValueSize =>

  val isPositive: Boolean = (value.last & 0x80.toByte) == 0

  override def toString: String = value.decimalString
}

object ImmediateValue {

  type ValueToByteImmediate = Byte => ImmediateValue with ByteSize
  type ValueToWordImmediate = Short => ImmediateValue with WordSize
  type ValueToDoubleWordImmediate = Int => ImmediateValue with DoubleWordSize
  type ValueToQuadWordImmediate = Long => ImmediateValue with QuadWordSize

  val byteImmediate: ValueToByteImmediate = value => new ImmediateValue(value.encodeLittleEndian) with ByteSize {}
  val wordImmediate: ValueToWordImmediate = value => new ImmediateValue(value.encodeLittleEndian) with WordSize {}
  val doubleWordImmediate: ValueToDoubleWordImmediate = value => new ImmediateValue(value.encodeLittleEndian) with DoubleWordSize {}
  val quadWordImmediate: ValueToQuadWordImmediate = value => new ImmediateValue(value.encodeLittleEndian) with QuadWordSize {}

  trait I8086Implicits {
    implicit val byteImm: ValueToByteImmediate = byteImmediate
    implicit val wordImm: ValueToWordImmediate = wordImmediate
  }

  trait I386Implicits {
    implicit val doubleWordImm: ValueToDoubleWordImmediate = doubleWordImmediate
  }

  trait X64Implicits {
    implicit val quadWordImm: ValueToQuadWordImmediate = quadWordImmediate
  }
}