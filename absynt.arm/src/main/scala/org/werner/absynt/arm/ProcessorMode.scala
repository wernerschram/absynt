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

package org.werner.absynt.arm

import org.werner.absynt.arm.instructions.{Branch, Breakpoint, ChangeProcessorState, DataProcessing, LoadStore, LoadStoreMultiple, MoveStatusRegister, Multiply, SoftwareInterrupt}
import org.werner.absynt.arm.operands.Shifter
import org.werner.absynt.arm.operands.registers.Register

sealed abstract class ProcessorMode {
  implicit val processorMode: ProcessorMode = this
}

object ProcessorMode {

  case object A32
    extends ProcessorMode
      with Register.ARMRegisters
      with Shifter.A32Shifter
      with Branch.A32Operations
      with Breakpoint.A32Operations
      with ChangeProcessorState.A32Operations
      with DataProcessing.A32Operations
      with LoadStoreMultiple.A32Operations
      with LoadStore.A32Operations
      with MoveStatusRegister.A32Operations
      with Multiply.A32Operations
      with SoftwareInterrupt.A32Operations

  case object Thumb extends ProcessorMode
}
