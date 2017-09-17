package assembler.x86.operations

import assembler.Label
import assembler.x86.ProcessorMode
import org.scalatest.{Matchers, WordSpec}

class X86OperationSuite extends WordSpec with Matchers {

  "an X86 instruction" when {
    "in protected mode" should {

      class MyInstruction extends X86Operation {
        override def code: List[Byte] = 0x00.toByte :: Nil

        override def mnemonic = "mis"

        override def operands = Nil

        override implicit val processorMode: ProcessorMode = ProcessorMode.Protected

        override val label: Label = Label.noLabel
      }

      "return the size of the instruction" in {
        val instruction = new MyInstruction()
        instruction.size should be(1)
      }
    }
  }
}