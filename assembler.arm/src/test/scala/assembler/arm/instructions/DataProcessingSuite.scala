package assembler.arm.instructions

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition
import assembler.arm.operands.Shifter
import assembler.arm.operands.Shifter._
import assembler.arm.operands.registers.GeneralRegister._
import assembler.memory.MemoryPage

class DataProcessingSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[ARMInstruction])

  "an AddCarry instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode adc r2, r0, r1" in {
        AddCarry(R0, R1, R2).encodeByte should be(Hex.msb("e0a02001"))
      }

      "correctly encode adcs r2, r0, r1" in {
        AddCarry.setFlags(R0, R1, R2).encodeByte should be(Hex.msb("e0b02001"))
      }

      "correctly encode adcge r2, r0, r1" in {
        AddCarry(R0, R1, R2, Condition.SignedGreaterOrEqual).encodeByte should be(Hex.msb("a0a02001"))
      }

      "correctly encode adc r2, r0, r1, lsl #2" in {
        AddCarry(R0, Shifter.LogicalLeftShift(R1, 2.toByte), R2).encodeByte should be(Hex.msb("e0a02101"))
      }

      "correctly represent adc r2, r0, r1, lsl #2 as a string" in {
        AddCarry(R0, Shifter.LogicalLeftShift(R1, 2.toByte), R2).toString should be("adc r2, r0, r1, lsl #2")
      }

      "throw an AssertionError for adc r2, r0, r1, lsl #33" in {
        an[AssertionError] should be thrownBy {
          AddCarry(R0, Shifter.LogicalLeftShift(R1, 33.toByte), R2)
        }
      }

      "correctly encode adc r3, r1, r14, lsl r5" in {
        AddCarry(R1, Shifter.LogicalLeftShift(R14, R5), R3).encodeByte should be(Hex.msb("e0a1351e"))
      }

      "correctly encode adc r9, r8, r6, lsl #4" in {
        AddCarry(R8, Shifter.LogicalRightShift(R6, 4.toByte), R9).encodeByte should be(Hex.msb("e0a89226"))
      }

      "correctly encode adc r5, r7, r10, lsr r11" in {
        AddCarry(R7, Shifter.LogicalRightShift(R10, R11), R5).encodeByte should be(Hex.msb("e0a75b3a"))
      }

      "correctly encode adc r15, r11, sp, asr #13" in {
        AddCarry(R11, Shifter.ArithmeticRightShift(SP, 13.toByte), R15).encodeByte should be(Hex.msb("e0abf6cd"))
      }

      "correctly encode adc r15, r11, r13, asr r13" in {
        AddCarry(R11, Shifter.ArithmeticRightShift(R13, R13), R15).encodeByte should be(Hex.msb("e0abfd5d"))
      }

      "correctly encode adc r0, r0, r0, ror #31" in {
        AddCarry(R0, Shifter.RightRotate(R0, 31.toByte), R0).encodeByte should be(Hex.msb("e0a00fe0"))
      }

      "correctly encode adc r9, r8, r7, asr r6" in {
        AddCarry(R8, Shifter.RightRotate(R7, R6), R9).encodeByte should be(Hex.msb("e0a89677"))
      }

      "correctly encode adc r1, r2, r3, rrx" in {
        AddCarry(R2, Shifter.RightRotateExtend(R3), R1).encodeByte should be(Hex.msb("e0a21063"))
      }

      "correctly encode adc r1, r2, 1, 2" in {
        AddCarry(R2, Shifter.RightRotateImmediate(1.toByte,2.toByte), R1).encodeByte should be(Hex.msb("e2a21101"))
      }

      info("Note that there are different ways of encoding adc r1, r2, #1073741824. This test could result in false negatives")
      "correctly encode adc r1, r2, #1073741824" in {
        AddCarry(R2, Shifter.ForImmediate(1073741824), R1).encodeByte should be(Hex.msb("e2a21101"))
      }

      "throw an AssertionError for adc r1, r2, 1, 1" in {
        an[AssertionError] should be thrownBy {
          AddCarry(R2, Shifter.RightRotateImmediate(1.toByte,1.toByte), R1)
        }
      }

      "throw an AssertionError for adc r1, r2, 1, 32" in {
        an[AssertionError] should be thrownBy {
          AddCarry(R2, Shifter.RightRotateImmediate(1.toByte,32.toByte), R1)
        }
      }
    }
  }

  info("All DataProcessing instructions inherrit the same class. Only the AddCarry instruction is tested extensively.")

  "an Add instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode add r2, r0, rr" in {
        Add(R0, R1, R2).encodeByte should be(Hex.msb("e0802001"))
      }
    }
  }

  "an And instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode and r2, r0, r1" in {
        And(R0, R1, R2).encodeByte should be(Hex.msb("e0002001"))
      }
    }
  }

  "an BitClear instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode and r2, r0, r1" in {
        BitClear(R0, R1, R2).encodeByte should be(Hex.msb("e1c02001"))
      }
    }
  }

  "a CompareNegative instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode cmn r0, r1" in {
        CompareNegative(R0, R1).encodeByte should be(Hex.msb("e1600001"))
      }

      "correctly represent r0, r1 as a string" in {
        CompareNegative(R0, R1).toString should be("cmn r0, r1")
      }

    }
  }

  "a Compare instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode cmp r2, r0, r1" in {
        Compare(R0, R1).encodeByte should be(Hex.msb("e1400001"))
      }
    }
  }

  "a Move instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode mov r2, r1" in {
        Move(R1, R2).encodeByte should be(Hex.msb("e1a02001"))
      }

      "correctly encode mov r4, 0x20200000 (word 0x20200000 cannot be encoded in one instruction)" in {
        // Because a dataprocessing instruction cannot encode 0x202000000, this instruction will be split up into two instructions as below:
        //          40:   e3a04580        mov     r4, #128, 10            ; 0x20000000
        //          44:   e3844980        orr     r4, r4, #128, 18        ; 0x200000
        Move.forShifters(0x20200000, R4).flatMap { instruction => instruction.encodeByte() } should be(Hex.msb("e3a04580 e3844980"))
      }

      "correctly encode mov r4, 0x0" in {
        // Because a dataprocessing instruction cannot encode 0x202000000, this instruction will be split up into two instructions as below:
        //          40:   e3a04580        mov     r4, #128, 10            ; 0x20000000
        //          44:   e3844980        orr     r4, r4, #128, 18        ; 0x200000
        Move.forShifters(0x0, R4).flatMap { instruction => instruction.encodeByte() } should be(Hex.msb("e3a04000"))
      }
    }
  }

  "an ExclusiveOr instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode eor r4, r5, r9 lsl #2" in {
        ExclusiveOr(R5, Shifter.LogicalLeftShift(R9, 2.toByte), R4).encodeByte should be(Hex.msb("e0254109"))
      }
    }
  }

  "a MoveNot instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode mvn r2, r1" in {
        MoveNot(R1, R2).encodeByte should be(Hex.msb("e1e02001"))
      }

      "correctly represent mvn r2, r1 as a string" in {
        MoveNot(R1, R2).toString should be("mvn r2, r1")
      }
    }
  }

  "an Or instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode orr r2, r0, r1" in {
        Or(R0, R1, R2).encodeByte should be(Hex.msb("e1802001"))
      }
    }
  }

  "a ReverseSubtract instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode rsb r2, r0, r1" in {
        ReverseSubtract(R0, R1, R2).encodeByte should be(Hex.msb("e0602001"))
      }
    }
  }

  "a ReverseSubtractCarry instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode rsc r2, r0, r1" in {
        ReverseSubtractCarry(R0, R1, R2).encodeByte should be(Hex.msb("e0e02001"))
      }
    }
  }

  "a SubtractCarry instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode sbc r2, r0, r1" in {
        SubtractCarry(R0, R1, R2).encodeByte should be(Hex.msb("e0c02001"))
      }
    }
  }

  "a Subtract instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode sub r2, r0, r1" in {
        Subtract(R0, R1, R2).encodeByte should be(Hex.msb("e0402001"))
      }
    }
  }

  "a TestEquivalence instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode teq r2, r0, r1" in {
        TestEquivalence(R0, R1).encodeByte should be(Hex.msb("e1200001"))
      }
    }
  }

  "a Test instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode tst r2, r0, r1" in {
        Test(R0, R1).encodeByte should be(Hex.msb("e1000001"))
      }
    }
  }

}