package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.operations.{ARMOperation, LabeledARMOperation}
import assembler.sections.Section
import org.scalatest.{Matchers, WordSpec}

class ARMInstructionSuite extends WordSpec with Matchers {

  implicit val page: Section = Section(List.empty[ARMOperation])

  "an ARM instruction" when {
    "in a32 mode" should {

      class MyInstruction extends ARMOperation {
        val opcode = "my"

        override def encodeWord()(implicit page: Section) = 4
      }

      "return the size of the instruction" in {
        val instruction = new MyInstruction()
        instruction.size() should be(4)
      }

      "return a labeled instruction for the given instruction" in {
        val instruction = new MyInstruction()

        val labeledInstruction = instruction.withLabel("Label")

        labeledInstruction.label.toString should be("Label")
      }

    }
  }

  "a Labeled ARM instruction" when {
    "in a32 mode" should {

      class MyInstruction extends ARMOperation {
        val opcode = "my"

        override def encodeWord()(implicit page: Section) = 0x12345678
      }

      "correctly return the size of the instruction" in {
        val instruction = new MyInstruction()

        val labeledInstruction = new LabeledARMOperation(instruction, "Label")
        labeledInstruction.size() should be(4)
      }

      "return the encoded instruction when encodeByte is called" in {
        val instruction = new MyInstruction()

        val labeledInstruction = new LabeledARMOperation(instruction, "Label")
        labeledInstruction.encodeByte() should be(0x78.toByte :: 0x56.toByte :: 0x34.toByte :: 0x12.toByte :: Nil)
      }

      "return the encoded instruction when encodeWord is called" in {
        val instruction = new MyInstruction()

        val labeledInstruction = new LabeledARMOperation(instruction, "Label")
        labeledInstruction.encodeWord() should be(0x12345678)
      }

      "correctly represent the instruction as a string" in {
        implicit val processorMode = ProcessorMode.A32

        val instruction = Breakpoint(4)

        val labeledInstruction = new LabeledARMOperation(instruction, "Label")
        labeledInstruction.toString() should be("Label: bkpt 4")
      }
    }
  }
}