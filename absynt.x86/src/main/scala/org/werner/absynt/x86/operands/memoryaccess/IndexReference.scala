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

import org.werner.absynt.x86.operands.{
  ByteSize,
  DoubleWordSize,
  QuadWordSize,
  ValueSize,
  WordDoubleQuadSize,
  WordSize
}
import org.werner.absynt.x86.operands.memoryaccess.MemoryLocation.BaseIndexReference
import org.werner.absynt.x86.operands.registers.{
  BasePointerRegister,
  DestinationIndex,
  GeneralPurposeRegister,
  IndexRegister,
  SourceIndex
}

class IndexReference {}

class DestinationReference[Size <: WordDoubleQuadSize](
    reference: BaseIndexReference[
      GeneralPurposeRegister with BasePointerRegister with Size,
      DestinationIndex with IndexRegister with Size,
      Size,
    ]
) extends RegisterMemoryLocation[Size](reference) {
  self: ValueSize =>
}

class SourceReference[Size <: WordDoubleQuadSize](
    reference: BaseIndexReference[
      Nothing,
      SourceIndex with IndexRegister with Size,
      Size,
    ]
) extends RegisterMemoryLocation(reference) {
  self: ValueSize =>
}

object IndexReference {

  abstract class IndexForSize[Size <: ValueSize] {

    def destinationReference[AddressSize <: WordDoubleQuadSize](
        reference: BaseIndexReference[
          Nothing,
          DestinationIndex with IndexRegister with AddressSize,
          AddressSize,
        ]
    ): DestinationReference[AddressSize] with Size

    def sourceReference[AddressSize <: WordDoubleQuadSize](
        reference: BaseIndexReference[
          Nothing,
          SourceIndex with IndexRegister with AddressSize,
          AddressSize,
        ]
    ): SourceReference[AddressSize] with Size
  }

  trait I8086Implicits {
    implicit def indexForByteSize: IndexForSize[ByteSize] =
      new IndexForSize[ByteSize] {
        override def destinationReference[AddressSize <: WordDoubleQuadSize](
            reference: BaseIndexReference[
              Nothing,
              DestinationIndex with IndexRegister with AddressSize,
              AddressSize,
            ]
        ): DestinationReference[AddressSize] with ByteSize =
          new DestinationReference[AddressSize](reference) with ByteSize

        override def sourceReference[AddressSize <: WordDoubleQuadSize](
            reference: BaseIndexReference[
              Nothing,
              SourceIndex with IndexRegister with AddressSize,
              AddressSize,
            ]
        ): SourceReference[AddressSize] with ByteSize =
          new SourceReference[AddressSize](reference) with ByteSize
      }

    implicit def IndexForWordSize: IndexForSize[WordSize] =
      new IndexForSize[WordSize] {
        override def destinationReference[AddressSize <: WordDoubleQuadSize](
            reference: BaseIndexReference[
              Nothing,
              DestinationIndex with IndexRegister with AddressSize,
              AddressSize,
            ]
        ): DestinationReference[AddressSize] with WordSize =
          new DestinationReference[AddressSize](reference) with WordSize

        override def sourceReference[AddressSize <: WordDoubleQuadSize](
            reference: BaseIndexReference[
              Nothing,
              SourceIndex with IndexRegister with AddressSize,
              AddressSize,
            ]
        ): SourceReference[AddressSize] with WordSize =
          new SourceReference[AddressSize](reference) with WordSize
      }
  }

  trait I386Implicits {
    implicit def IndexForDoubleWordSize: IndexForSize[DoubleWordSize] =
      new IndexForSize[DoubleWordSize] {
        override def destinationReference[AddressSize <: WordDoubleQuadSize](
            reference: BaseIndexReference[
              Nothing,
              DestinationIndex with IndexRegister with AddressSize,
              AddressSize,
            ]
        ): DestinationReference[AddressSize] with DoubleWordSize =
          new DestinationReference[AddressSize](reference) with DoubleWordSize

        override def sourceReference[AddressSize <: WordDoubleQuadSize](
            reference: BaseIndexReference[
              Nothing,
              SourceIndex with IndexRegister with AddressSize,
              AddressSize,
            ]
        ): SourceReference[AddressSize] with DoubleWordSize =
          new SourceReference[AddressSize](reference) with DoubleWordSize
      }
  }

  trait X64Implicits {
    implicit def IndexForQuadWordSize: IndexForSize[QuadWordSize] =
      new IndexForSize[QuadWordSize] {
        override def destinationReference[AddressSize <: WordDoubleQuadSize](
            reference: BaseIndexReference[
              Nothing,
              DestinationIndex with IndexRegister with AddressSize,
              AddressSize,
            ]
        ): DestinationReference[AddressSize] with QuadWordSize =
          new DestinationReference[AddressSize](reference) with QuadWordSize

        override def sourceReference[AddressSize <: WordDoubleQuadSize](
            reference: BaseIndexReference[
              Nothing,
              SourceIndex with IndexRegister with AddressSize,
              AddressSize,
            ]
        ): SourceReference[AddressSize] with QuadWordSize =
          new SourceReference[AddressSize](reference) with QuadWordSize
      }
  }

}
