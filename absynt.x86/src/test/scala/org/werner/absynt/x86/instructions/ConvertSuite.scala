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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.werner.absynt.Hex
import org.werner.absynt.x86.ProcessorMode

class ConvertSuite extends AnyWordSpec with Matchers {

  "an Convert instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode cbw" in {
        Convert.ScaleUp(AL).encodeByte should be(Hex.lsb("98"))
      }
      "correctly represent cbw as a string" in {
        Convert.ScaleUp(AL).toString should be("cbw ax")
      }

      "correctly encode cwd" in {
        Convert.Split(AX).encodeByte should be(Hex.lsb("99"))
      }
      "correctly represent cwd as a string" in {
        Convert.Split(AX).toString should be("cwd ax")
      }
    }

    "in protected mode" should {
      import ProcessorMode.Protected._
      "correctly encode cbw" in {
        Convert.ScaleUp(AL).encodeByte should be(Hex.lsb("66 98"))
      }
      "correctly represent cbw as a string" in {
        Convert.ScaleUp(AL).toString should be("cbw ax")
      }

      "correctly encode cwde" in {
        Convert.ScaleUp(AX).encodeByte should be(Hex.lsb("98"))
      }
      "correctly represent cwde as a string" in {
        Convert.ScaleUp(AX).toString should be("cwde eax")
      }

      "correctly encode cwd" in {
        Convert.Split(AX).encodeByte should be(Hex.lsb("66 99"))
      }
      "correctly represent cwd as a string" in {
        Convert.Split(AX).toString should be("cwd ax")
      }

      "correctly encode cdq" in {
        Convert.Split(EAX).encodeByte should be(Hex.lsb("99"))
      }
      "correctly represent cdq as a string" in {
        Convert.Split(EAX).toString should be("cdq eax")
      }
    }
  }

  "in long mode" should {
    import ProcessorMode.Long._
    "correctly encode cbw" in {
      Convert.ScaleUp(AL).encodeByte should be(Hex.lsb("66 98"))
    }
    "correctly represent cbw as a string" in {
      Convert.ScaleUp(AL).toString should be("cbw ax")
    }

    "correctly encode cwde" in {
      Convert.ScaleUp(AX).encodeByte should be(Hex.lsb("98"))
    }
    "correctly represent cwde as a string" in {
      Convert.ScaleUp(AX).toString should be("cwde eax")
    }

    "correctly encode cdqe" in {
      Convert.ScaleUp(EAX).encodeByte should be(Hex.lsb("48 98"))
    }
    "correctly represent cdqe as a string" in {
      Convert.ScaleUp(EAX).toString should be("cdqe rax")
    }

    "correctly encode cwd" in {
      Convert.Split(AX).encodeByte should be(Hex.lsb("66 99"))
    }
    "correctly represent cwd as a string" in {
      Convert.Split(AX).toString should be("cwd ax")
    }

    "correctly encode cdq" in {
      Convert.Split(EAX).encodeByte should be(Hex.lsb("99"))
    }
    "correctly represent cdq as a string" in {
      Convert.Split(EAX).toString should be("cdq eax")
    }

    "correctly encode cqo" in {
      Convert.Split(RAX).encodeByte should be(Hex.lsb("48 99"))
    }
    "correctly represent cqo as a string" in {
      Convert.Split(RAX).toString should be("cqo rax")
    }
  }
}