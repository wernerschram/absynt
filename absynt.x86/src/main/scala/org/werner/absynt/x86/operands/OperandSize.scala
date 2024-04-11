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

sealed trait OperandSize {
  def sizeEquals(that: OperandSize): Boolean
}

sealed trait ValueSize extends OperandSize {
  def sizeName: String
}

sealed trait ByteWordDoubleSize extends ValueSize // 8, 16, 32

sealed trait WordDoubleQuadSize extends ValueSize //16, 32, 64

sealed trait WordDoubleSize extends ByteWordDoubleSize with WordDoubleQuadSize //16, 32

sealed trait WordQuadSize extends WordDoubleQuadSize //16, 64

sealed trait DoubleQuadSize extends WordDoubleQuadSize //32, 64

trait ByteSize extends ValueSize with ByteWordDoubleSize {
  override val sizeName = "BYTE"
  final override def sizeEquals(that: OperandSize): Boolean = that.isInstanceOf[ByteSize]
}

trait WordSize extends WordDoubleSize with WordQuadSize {
  override val sizeName = "WORD"
  final override def sizeEquals(that: OperandSize): Boolean = that.isInstanceOf[WordSize]
}

trait DoubleWordSize extends WordDoubleSize with DoubleQuadSize {
  override val sizeName = "DWORD"
  final override def sizeEquals(that: OperandSize): Boolean = that.isInstanceOf[DoubleWordSize]
}

trait QuadWordSize extends DoubleQuadSize with WordQuadSize {
  override val sizeName = "QWORD"
  final override def sizeEquals(that: OperandSize): Boolean = that.isInstanceOf[QuadWordSize]
}

sealed trait FarPointerSize[OffsetSize<:WordDoubleSize] extends OperandSize

trait FarWordSize extends FarPointerSize[WordSize] {
  final override def sizeEquals(that: OperandSize): Boolean = that.isInstanceOf[FarWordSize]
}

trait FarDoubleWordSize extends FarPointerSize[DoubleWordSize] {
  final override def sizeEquals(that: OperandSize): Boolean = that.isInstanceOf[FarDoubleWordSize]
}
