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

package org.werner.absynt.x86.operands.memoryaccess

import org.werner.absynt.ListExtensions._
import org.werner.absynt.x86.operands._

sealed abstract class NearPointer(val offset: Seq[Byte])
  extends Operand {
  self: ValueSize =>

  def encodeBytes: Seq[Byte] = offset
}

object ShortPointer {
  def apply(offset: Byte): NearPointer & ByteSize =
    new NearPointer(offset.encodeLittleEndian) with ByteSize {
      override def toString: String = s"0x${this.offset.bigEndianHexString}"
    }
}

object LongPointer {
  def realMode(offset: Int): NearPointer & WordSize = {
    assume(offset.toShort == offset)
    new NearPointer(offset.toShort.encodeLittleEndian) with WordSize {
      override def toString: String = s"0x${this.offset.bigEndianHexString}"
    }
  }

  def protectedMode(offset: Int): NearPointer & DoubleWordSize =
     new NearPointer(offset.encodeLittleEndian) with DoubleWordSize {
       override def toString: String = s"0x${this.offset.bigEndianHexString}"
    }
}