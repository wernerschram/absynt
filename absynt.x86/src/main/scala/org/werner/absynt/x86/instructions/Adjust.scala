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

package org.werner.absynt.x86.instructions

import org.werner.absynt.x86.{ArchitectureBounds, ProcessorMode}
import org.werner.absynt.x86.operands.{Accumulator, ByteSize, ImmediateValue}
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder.{OperandOrder, destination}
import org.werner.absynt.x86.operations.*

object Adjust {
  trait Operations {
    self: ArchitectureBounds & ProcessorMode & OperandSizeInfo =>
    private def Static(code: Byte, mnemonic: String) =
      new Static(code :: Nil, mnemonic) with NoDisplacement with NoImmediate

    private def StaticImm(code: Byte, mnemonic: String, immediateValue: ImmediateValue[?] & ByteSize) =
      new Static(code :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize](immediateValue, destination)

    object AdjustAfterAddition {
      def ascii(destination: Accumulator.LowByte.type): X86Operation = Static(0x37.toByte, "aaa")
      def decimal(destination: Accumulator.LowByte.type): X86Operation = Static(0x27.toByte, "daa")
    }

    object AdjustBeforeDivide {
      def ascii(destination: Accumulator.Word.type): X86Operation = base(ImmediateValue.byteImmediate(10.toByte))
      private def base(adjustmentBase: ImmediateValue[?] & ByteSize): X86Operation = StaticImm(0xD5.toByte, "aad", adjustmentBase)
      def base(destination: Accumulator.Word.type, adjustmentBase: ImmediateValue[?] & ByteSize): X86Operation = base(adjustmentBase)
    }

    object AdjustAfterMultiply {
      def ascii(destination: Accumulator.Word.type): X86Operation = base(ImmediateValue.byteImmediate(10.toByte))
      private def base(adjustmentBase: ImmediateValue[?] & ByteSize): X86Operation = StaticImm(0xD4.toByte, "aam", adjustmentBase)
      def base(destination: Accumulator.Word.type, adjustmentBase: ImmediateValue[?] & ByteSize): X86Operation = base(adjustmentBase)
    }

    object AdjustAfterSubtraction {
      def ascii(destination: Accumulator.LowByte.type): X86Operation = Static(0x3F.toByte, "aas")
      def decimal(destination: Accumulator.LowByte.type): X86Operation = Static(0x2F.toByte, "das")
    }
  }
}
