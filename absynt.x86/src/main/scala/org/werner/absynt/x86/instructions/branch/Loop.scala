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

package org.werner.absynt.x86.instructions.branch

import org.werner.absynt.Label
import org.werner.absynt.resource.{RelativeReference, Resource, UnlabeledEncodable}
import org.werner.absynt.x86._
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operands.memoryaccess._
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder._
import org.werner.absynt.x86.operations.branch.{JumpOption, LabelJumpOperation}
import org.werner.absynt.x86.operations.{NoImmediate, OperandSizeInfo, OperandWithOperandSizePrefixInfo, Static, X86Operation, NearPointer => NearPointerOperation}

object Loop {

  trait Operations {
    self: ArchitectureBounds with OperandSizeInfo =>

    sealed abstract class LoopOperations(val shortOpcode: Seq[Byte], implicit val mnemonic: String) {

      protected def Rel8(nearPointer: NearPointer with ByteSize): Static with NearPointerOperation[ByteSize] with NoImmediate =
        new Static(shortOpcode, mnemonic) with NearPointerOperation[ByteSize] with NoImmediate {
          override val pointer: OperandWithOperandSizePrefixInfo[NearPointer with ByteSize] = nearPointer

          override def pointerOrder: OperandOrder = destination
        }

      def apply(targetLabel: Label): RelativeReference = {
        LabelJumpOperation(
          Seq(new JumpOption(2, Byte.MinValue, Byte.MaxValue) {
            override def encodableForPointer(offset: Int): Resource with UnlabeledEncodable =
              Rel8(ShortPointer(offset.toByte))
          }),
          mnemonic,
          targetLabel
        )
      }

      def apply(nearPointer: NearPointer with ByteSize): X86Operation =
        Rel8(nearPointer)
    }

    object Loop extends LoopOperations(0xE2.toByte :: Nil, "loop") {
      object Equal extends LoopOperations(0xE1.toByte :: Nil, "loope")
      object NotEqual extends LoopOperations(0xE0.toByte :: Nil, "loopne")
    }
  }
}
