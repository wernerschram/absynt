package assembler.x86.operations

import assembler.{Designation, Encodable}
import assembler.sections.Section
import assembler.x86.ProcessorMode
import assembler.x86.instructions.Interrupt
import assembler.x86.operands.Register
import assembler.x86.operands.Register.AX
import org.scalatest.{Matchers, WordSpec}

class X86OperationSuite extends WordSpec with Matchers {

  implicit val page: Section = Section(List.empty[Designation[Encodable]])

  "an X86 instruction" when {
    "in protected mode" should {

      class MyInstruction extends X86Operation {
        override def code: List[Byte] = 0x00.toByte :: Nil

        override def mnemonic = "mis"

        override def operands = Nil

        override implicit val processorMode: ProcessorMode = ProcessorMode.Protected
      }

      "return the size of the instruction" in {
        val instruction = new MyInstruction()
        instruction.size() should be(1)
      }
    }
  }
}