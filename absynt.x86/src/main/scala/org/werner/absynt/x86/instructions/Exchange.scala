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
import org.werner.absynt.x86.operands.*
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder.OperandOrder
import org.werner.absynt.x86.operations.*

object Exchange {

  trait Operations {
    self: ArchitectureBounds & ProcessorMode & OperandSizeInfo =>
    private val mnemonic = "xchg"

    private def AxToR16[Size <: MaxWideSize](source: AccumulatorRegister & Size, destination: GeneralPurposeRegister & Size): X86Operation =
      new RegisterEncoded(destination, 0x90.toByte :: Nil, OperandOrder.destination, mnemonic)
        with NoDisplacement 
        with NoImmediate 
        with ExtraOperands(OperandInfo.implicitOperand(source, OperandOrder.source)(noOperandSizePrefixRequirement))

    private def R16ToAX[Size <: MaxWideSize](source: GeneralPurposeRegister & Size, destination: AccumulatorRegister & Size): X86Operation =
      new RegisterEncoded(source, 0x90.toByte :: Nil, OperandOrder.source, mnemonic) 
        with NoDisplacement 
        with NoImmediate 
        with ExtraOperands(OperandInfo.implicitOperand(destination, OperandOrder.destination)(noOperandSizePrefixRequirement))

    private def R16ToRM16[Size <: MaxWideSize](source: GeneralPurposeRegister & Size, destination: ModRMEncodableOperand & Size) =
      new ModRRM(source, destination, 0x86.toByte :: Nil, mnemonic, OperandOrder.destination)

    private def RM16ToR16[Size <: MaxWideSize](source: ModRMEncodableOperand & Size, destination: GeneralPurposeRegister & Size) =
      new ModRRM(destination, source, 0x86.toByte :: Nil, mnemonic, OperandOrder.source)



    object Exchange {
      def apply[Size <: MaxWideSize](source: AccumulatorRegister & Size, destination: GeneralPurposeRegister & Size): X86Operation =
        AxToR16(source, destination)


      def apply[Size <: MaxWideSize](source: GeneralPurposeRegister & Size, destination: AccumulatorRegister & Size): X86Operation =
        R16ToAX(source, destination)

      def apply[Size <: MaxWideSize](source: GeneralPurposeRegister & Size, destination: ModRMEncodableOperand & Size): X86Operation =
        R16ToRM16(source, destination)


      def apply[Size <: MaxWideSize](source: ModRMEncodableOperand & Size, destination: GeneralPurposeRegister & Size): X86Operation =
        RM16ToR16(source, destination)
    }
  }
}
