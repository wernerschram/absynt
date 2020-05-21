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

import org.werner.absynt.x86.operations.{NoDisplacement, NoImmediate, Static, X86Operation}

object Generic {
  trait LegacyOperations {
    object Halt {
      def apply(): X86Operation = new Static(0xF4.toByte :: Nil, "hlt") with NoDisplacement with NoImmediate
    }

    object ComplementCarry {
      def apply(): X86Operation = new Static(0xF5.toByte :: Nil, "cmc") with NoDisplacement with NoImmediate
    }
  }

  trait I386Operations extends LegacyOperations {
    object Lock {
      def apply(): X86Operation = new Static(0xF0.toByte :: Nil, "lock") with NoDisplacement with NoImmediate
    }
  }
}
