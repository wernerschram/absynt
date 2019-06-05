package org.werner.absynt.x86.operations

import org.scalatest.{Matchers, WordSpec}

class X86OperationSuite extends WordSpec with Matchers {

  "an X86 instruction" when {
    "in protected mode" should {

      class MyInstruction extends X86Operation(0x00.toByte :: Nil) with NoModRM with NoImmediate with NoDisplacement {
        override def mnemonic = "mis"
      }

      "return the size of the instruction" in {
        val instruction = new MyInstruction()
        instruction.size should be(1)
      }
    }
  }
}