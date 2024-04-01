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
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder
import org.werner.absynt.x86.operations._

object Convert {

  sealed trait Common {
    self: ArchitectureBounds & ProcessorMode & OperandSizeInfo =>

    protected def ALToAX(): X86Operation =
      new Static(0x98.toByte :: Nil, "cbw") with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Accumulator.Word, OperandOrder.source)
      }

    protected def AXToDXAX(): X86Operation =
      new Static(0x99.toByte :: Nil, "cwd") with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Accumulator.Word, OperandOrder.source)
      }

    protected def AXToEAX(): X86Operation =
      new Static(0x98.toByte :: Nil, "cwde") with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Accumulator.DoubleWord, OperandOrder.source)
      }

    protected def EAXToEDXEAX(): X86Operation =
      new Static(0x99.toByte :: Nil, "cdq") with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Accumulator.DoubleWord, OperandOrder.source)
      }

    protected def EAXToRAX(): X86Operation =
      new Static(0x98.toByte :: Nil, "cdqe") with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Accumulator.QuadWord, OperandOrder.source)
      }

    protected def RAXToRDXRAX(): X86Operation =
      new Static(0x99.toByte :: Nil, "cqo") with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Accumulator.QuadWord, OperandOrder.source)
      }
  }

  trait LegacyOperations extends Common {
    self: ProcessorMode.LegacyBounds & ProcessorMode & OperandSizeInfo =>

    object Convert {
      object ScaleUp {
        def apply(accumulator: Accumulator.LowByte.type): X86Operation = ALToAX()
      }

      object Split {
        def apply(accumulator: Accumulator.Word.type): X86Operation = AXToDXAX()
      }
    }
  }

  trait I386Operations extends Common {
    self: ProcessorMode.I386Bounds & ProcessorMode & OperandSizeInfo =>

    object Convert {
      object ScaleUp {
        def apply(accumulator: Accumulator.LowByte.type): X86Operation = ALToAX()
        def apply(accumulator: Accumulator.Word.type): X86Operation = AXToEAX()
      }

      object Split {
        def apply(accumulator: Accumulator.Word.type): X86Operation = AXToDXAX()
        def apply(accumulator: Accumulator.DoubleWord.type): X86Operation = EAXToEDXEAX()
      }
    }
  }

  trait LongOperations extends Common {
    self: ProcessorMode.LongBounds & ProcessorMode & OperandSizeInfo =>

    object Convert {
      object ScaleUp {
        def apply(accumulator: Accumulator.LowByte.type): X86Operation = ALToAX()
        def apply(accumulator: Accumulator.Word.type): X86Operation = AXToEAX()
        def apply(accumulator: Accumulator.DoubleWord.type): X86Operation = EAXToRAX()
      }

      object Split {
        def apply(accumulator: Accumulator.Word.type): X86Operation = AXToDXAX()
        def apply(accumulator: Accumulator.DoubleWord.type): X86Operation = EAXToEDXEAX()
        def apply(accumulator: Accumulator.QuadWord.type): X86Operation = RAXToRDXRAX()
      }
    }
  }
}
