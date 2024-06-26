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
import org.werner.absynt.x86.*
import org.werner.absynt.x86.operands.*
import org.werner.absynt.x86.operands.memoryaccess.*
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder.*
import org.werner.absynt.x86.operations.branch.{JumpOption, LabelJumpOperation}
import org.werner.absynt.x86.operations.{NoImmediate, OperandSizeInfo, Static, X86Operation, NearPointer as NearPointerOperation}

import scala.language.implicitConversions

object Loop {

  trait Operations {
    self: ArchitectureBounds & ProcessorMode & OperandSizeInfo =>

    sealed abstract class LoopOperations(val shortOpcode: Seq[Byte], val mnemonic: String) {

      protected def Rel8(nearPointer: NearPointer & ByteSize): Static & NearPointerOperation[ByteSize] & NoImmediate =
        new Static(shortOpcode, mnemonic) 
          with NearPointerOperation[ByteSize](nearPointer, destination) 
          with NoImmediate

      def apply(targetLabel: Label): RelativeReference = {
        LabelJumpOperation(
          Seq(new JumpOption(2, Byte.MinValue, Byte.MaxValue) {
            override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
              Rel8(ShortPointer(offset.toByte))
          }),
          mnemonic,
          targetLabel
        )
      }

      def apply(nearPointer: NearPointer & ByteSize): X86Operation =
        Rel8(nearPointer)
    }

    object Loop extends LoopOperations(0xE2.toByte :: Nil, "loop") {
      object Equal extends LoopOperations(0xE1.toByte :: Nil, "loope")
      object NotEqual extends LoopOperations(0xE0.toByte :: Nil, "loopne")
    }
  }
}
