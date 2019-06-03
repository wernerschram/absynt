package org.werner.absynt.arm.instructions

import org.werner.absynt.arm.operations.ARMOperation
import org.scalatest.{Matchers, WordSpec}

class ARMInstructionSuite extends WordSpec with Matchers {

  "an ARM instruction" when {
    "in a32 mode" should {

      class MyInstruction extends ARMOperation {
        val opcode = "my"

        override def encodeWord = 4
      }

      "return the size of the instruction" in {
        val instruction = new MyInstruction()
        instruction.size should be(4)
      }

    }
  }
}