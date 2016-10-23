package assembler.arm.instructions

import org.scalamock.scalatest.MockFactory
import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec
import assembler.memory.MemoryPage
import assembler.arm.ProcessorMode

class ARMInstructionSuite extends WordSpec with ShouldMatchers with MockFactory {

  implicit val page: MemoryPage = new MemoryPage(List.empty[ARMInstruction])

  "an ARM instruction" when {
    "in a32 mode" should {

      class MyInstruction extends ARMInstruction {
        override def encodeWord()(implicit page: MemoryPage) = 4
      }

      "return the size of the instruction" in {
        val instruction = new MyInstruction()
        instruction.size() should be(4)
      }

      "return a labeled instruction for the given instruction" in {
        val instruction = new MyInstruction()

        val labeledInstruction = instruction.withLabel("Label")

        labeledInstruction.label.toString() should be("Label")
      }

    }
  }

  "a Labeled ARM instruction" when {
    "in a32 mode" should {

      "correctly return the size of the instruction" in {
        val instruction = stub[ARMInstruction]
        (instruction.size()(_: MemoryPage)).when(*).returns(4)

        val labeledInstruction = new LabeledARMInstruction(instruction, "Label")
        labeledInstruction.size() should be(4)
      }

      "return the encoded instruction when encodeByte is called" in {
        val instruction = stub[ARMInstruction]
        (instruction.encodeByte()(_: MemoryPage)).when(*).returns(0x01.toByte :: 0x02.toByte :: Nil)

        val labeledInstruction = new LabeledARMInstruction(instruction, "Label")
        labeledInstruction.encodeByte() should be(0x01.toByte :: 0x02.toByte :: Nil)
      }

      "return the encoded instruction when encodeWord is called" in {
        val instruction = stub[ARMInstruction]
        (instruction.encodeWord()(_: MemoryPage)).when(*).returns(0x12345678)

        val labeledInstruction = new LabeledARMInstruction(instruction, "Label")
        labeledInstruction.encodeWord() should be(0x12345678)
      }

      "correctly represent the instruction as a string" in {
        implicit val processorMode = ProcessorMode.A32

        val instruction = Breakpoint(4)

        val labeledInstruction = new LabeledARMInstruction(instruction, "Label")
        labeledInstruction.toString() should be("Label: bkpt 4")
      }
}
  }
}