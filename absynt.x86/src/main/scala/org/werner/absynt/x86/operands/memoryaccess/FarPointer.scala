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

sealed abstract case class FarPointer[Size<:WordDoubleSize](segment: ImmediateValue[Short] & WordSize, offset: ImmediateValue[?] & Size)
  extends Operand {
  self: FarPointerSize[Size] =>

  override def toString: String =
    s"FAR 0x${segment.encodedValue.bigEndianHexString}:0x${offset.encodedValue.bigEndianHexString}"

  def encodeByte: Seq[Byte] = offset.encodedValue ++ segment.encodedValue
}

object FarPointer {
  trait FarPointerForSize[OffsetSize<:WordDoubleSize]:
    extension (segment: ImmediateValue[Short] & WordSize) def instance(offset: ImmediateValue[?] & OffsetSize): FarPointer[OffsetSize] & FarPointerSize[OffsetSize]

  trait I8086Implicits {
    given FarPointerForSize[WordSize] with
      extension (segment: ImmediateValue[Short] & WordSize) override def instance(offset: ImmediateValue[?] & WordSize): FarPointer[WordSize] & FarPointerSize[WordSize] =
        new FarPointer[WordSize](segment, offset) with FarWordSize
  }


  trait I386Implicits {
    given FarPointerForSize[DoubleWordSize] with
      extension (segment: ImmediateValue[Short] & WordSize) override def instance(offset: ImmediateValue[?] & DoubleWordSize) =
        new FarPointer[DoubleWordSize](segment, offset) with FarDoubleWordSize
  }

  def apply[Size<:WordDoubleSize: FarPointerForSize](segment: ImmediateValue[Short] & WordSize, offset: ImmediateValue[?] & Size): FarPointer[Size] & FarPointerSize[Size] =
    segment.instance(offset)
}
