package assembler.arm.instructions

import assembler.{Encodable, Label}
import assembler.arm.ProcessorMode
import assembler.arm.operations.ARMOperation
import assembler.sections.Section
import org.scalatest.{Matchers, WordSpec}

class ARMInstructionSuite extends WordSpec with Matchers {

  implicit val page: Section = Section(List.empty[Encodable], 0)

  "an ARM instruction" when {
    "in a32 mode" should {

      class MyInstruction extends ARMOperation {
        val opcode = "my"

        override def encodeWord()(implicit page: Section) = 4

        val label: Label = Label.noLabel
      }

      "return the size of the instruction" in {
        val instruction = new MyInstruction()
        instruction.size() should be(4)
      }

    }
  }
}