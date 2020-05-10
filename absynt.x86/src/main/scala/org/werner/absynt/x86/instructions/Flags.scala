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

import org.werner.absynt.x86.operations.{NoDisplacement, NoImmediate, Static}

object Flags {
  trait Operations {
    object SetCarryFlag {
      def apply(): Static =
        new Static(0xF9.toByte :: Nil, "stc") with NoDisplacement with NoImmediate
    }

    object SetDirectionFlag {
      def apply(): Static =
        new Static(0xFD.toByte :: Nil, "std") with NoDisplacement with NoImmediate
    }

    object SetInterruptFlag {
      def apply(): Static =
        new Static(0xFB.toByte :: Nil, "sti") with NoDisplacement with NoImmediate
    }

    object ClearCarryFlag {
      def apply(): Static =
        new Static(0xF8.toByte :: Nil, "clc") with NoDisplacement with NoImmediate
    }

    object ClearDirectionFlag {
      def apply(): Static =
        new Static(0xFC.toByte :: Nil, "cld") with NoDisplacement with NoImmediate
    }

    object ClearInterruptFlag {
      def apply(): Static =
        new Static(0xFA.toByte :: Nil, "cli") with NoDisplacement with NoImmediate
    }
  }
}
