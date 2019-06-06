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

sealed abstract case class FarPointer[Size<:WordDoubleSize](segment: ImmediateValue with WordSize, offset: ImmediateValue with Size)
  extends Operand {
  self: FarPointerSize[Size] =>

  override def toString =
    s"FAR 0x${segment.value.bigEndianHexString}:0x${offset.value.bigEndianHexString}"

  def encodeByte: Seq[Byte] = offset.value ++ segment.value
}

object FarPointer {
  abstract class FarPointerForSize[OffsetSize<:WordDoubleSize] {
    def instance(segment: ImmediateValue with WordSize, offset: ImmediateValue with OffsetSize): FarPointer[OffsetSize] with FarPointerSize[OffsetSize]
  }

  trait I8086Implicits {
    implicit def FarPointerForWord: FarPointerForSize[WordSize] =
      (segment: ImmediateValue with WordSize, offset: ImmediateValue with WordSize) =>
        new FarPointer[WordSize](segment, offset) with FarWordSize
  }


  trait I386Implicits {
    implicit def FarPointerForDoubleWord: FarPointerForSize[DoubleWordSize] =
      (segment: ImmediateValue with WordSize, offset: ImmediateValue with DoubleWordSize) =>
        new FarPointer[DoubleWordSize](segment, offset) with FarDoubleWordSize
  }

  def apply[Size<:WordDoubleSize: FarPointerForSize](segment: ImmediateValue with WordSize, offset: ImmediateValue with Size): FarPointer[Size] with FarPointerSize[Size] =
    implicitly[FarPointerForSize[Size]].instance(segment, offset)
}
