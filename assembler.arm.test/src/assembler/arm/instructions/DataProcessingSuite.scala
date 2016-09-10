package assembler.arm.instructions

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec
import assembler.MemoryPage
import assembler.Hex
import assembler.arm.ProcessorMode
import assembler.arm.operands.registers.GeneralRegister._
import assembler.arm.operands.Condition._
import assembler.arm.operands._
import assembler.arm.instructions._
import assembler.arm.operands.Shifter.apply
class DataProcessingSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[ARMInstruction])

  "an AddCarry instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode adc r2, r0, r1" in {
        AddCarry(R0, R1, R2).encodeByte should be(Hex.MSB("e0a02001"))
      }

      "correctly encode adcs r2, r0, r1" in {
        AddCarry.setFlags(R0, R1, R2).encodeByte should be(Hex.MSB("e0b02001"))
      }

      "correctly encode adcge r2, r0, r1" in {
        AddCarry(R0, R1, R2, Condition.SignedGreaterOrEqual).encodeByte should be(Hex.MSB("a0a02001"))
      }
      
      "correctly encode adc r2, r0, r1, lsl #2" in {
        AddCarry(R0, Shifter.LogicalLeftShift(R1, 2.toByte), R2).encodeByte should be(Hex.MSB("e0a02101"))
      }

      "throw an AssertionError for adc r2, r0, r1, lsl #33" in {
        an[AssertionError] should be thrownBy {
          AddCarry(R0, Shifter.LogicalLeftShift(R1, 33.toByte), R2)
        }
      }

      "correctly encode adc r3, r1, r14, lsl r5" in {
        AddCarry(R1, Shifter.LogicalLeftShift(R14, R5), R3).encodeByte should be(Hex.MSB("e0a1351e"))
      }

      "correctly encode adc r9, r8, r6, lsl #4" in {
        AddCarry(R8, Shifter.LogicalRightShift(R6, 4.toByte), R9).encodeByte should be(Hex.MSB("e0a89226"))
      }

      "correctly encode adc r5, r7, r10, lsr r11" in {
        AddCarry(R7, Shifter.LogicalRightShift(R10, R11), R5).encodeByte should be(Hex.MSB("e0a75b3a"))
      }

      "correctly encode adc r15, r11, r13, asr #13" in {
        AddCarry(R11, Shifter.ArithmeticRightShift(R13, 13.toByte), R15).encodeByte should be(Hex.MSB("e0abf6cd"))
      }

      "correctly encode adc r15, r11, r13, asr r13" in {
        AddCarry(R11, Shifter.ArithmeticRightShift(R13, R13), R15).encodeByte should be(Hex.MSB("e0abfd5d"))
      }

      "correctly encode adc r0, r0, r0, ror #31" in {
        AddCarry(R0, Shifter.RightRotate(R0, 31.toByte), R0).encodeByte should be(Hex.MSB("e0a00fe0"))
      }

      "correctly encode adc r9, r8, r7, asr r6" in {
        AddCarry(R8, Shifter.RightRotate(R7, R6), R9).encodeByte should be(Hex.MSB("e0a89677"))
      }

      "correctly encode adc r1, r2, r3, rrx" in {
        AddCarry(R2, Shifter.RightRotateExtend(R3), R1).encodeByte should be(Hex.MSB("e0a21063"))
      }

      "correctly encode adc r1, r2, #1073741824" in {
        AddCarry(R2, Shifter.RightRotateImmediate(1.toByte,2.toByte), R1).encodeByte should be(Hex.MSB("e2a21101"))
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
        Add(R0, R1, R2).encodeByte should be(Hex.MSB("e0802001"))
      }
    }
  }
  
  "an And instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode and r2, r0, r1" in {
        And(R0, R1, R2).encodeByte should be(Hex.MSB("e0002001"))
      }
    }
  }
  
  "an BitClear instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode and r2, r0, r1" in {
        BitClear(R0, R1, R2).encodeByte should be(Hex.MSB("e1c02001"))
      }
    }
  }
  
  "a CompareNegative instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode cmn r2, r0, r1" in {
        CompareNegative(R0, R1).encodeByte should be(Hex.MSB("e1600001"))
      }
    }
  }
  
  "a Compare instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode cmp r2, r0, r1" in {
        Compare(R0, R1).encodeByte should be(Hex.MSB("e1400001"))
      }
    }
  }
    
  "a Move instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode mov r2, r1" in {
        Move(R1, R2).encodeByte should be(Hex.MSB("e1a02001"))
      }
    }
  }
   
  "an ExclusiveOr instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32
      
      "correctly encode eor r4, r5, r9 lsl #2" in {
        ExclusiveOr(R5, Shifter.LogicalLeftShift(R9, 2.toByte), R4).encodeByte should be(Hex.MSB("e0254109"))
      }
    }
  }

  "a MoveNot instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode mvn r2, r1" in {
        MoveNot(R1, R2).encodeByte should be(Hex.MSB("e1e02001"))
      }
    }
  }

  "an Or instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32
 
      "correctly encode orr r2, r0, r1" in {
        Or(R0, R1, R2).encodeByte should be(Hex.MSB("e1802001"))
      }
    }
  }

  "a ReverseSubtract instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32
 
      "correctly encode rsb r2, r0, r1" in {
        ReverseSubtract(R0, R1, R2).encodeByte should be(Hex.MSB("e0602001"))
      }
    }
  }

  "a ReverseSubtractCarry instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode rsc r2, r0, r1" in {
        ReverseSubtractCarry(R0, R1, R2).encodeByte should be(Hex.MSB("e0e02001"))
      }
    }
  }

  "a SubtractCarry instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode sbc r2, r0, r1" in {
        SubtractCarry(R0, R1, R2).encodeByte should be(Hex.MSB("e0c02001"))
      }
    }
  }

  "a Subtract instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode sub r2, r0, r1" in {
        Subtract(R0, R1, R2).encodeByte should be(Hex.MSB("e0402001"))
      }
    }
  }
  
  "a TestEquivalence instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode teq r2, r0, r1" in {
        TestEquivalence(R0, R1).encodeByte should be(Hex.MSB("e1200001"))
      }
    }
  }

  "a Test instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode tst r2, r0, r1" in {
        Test(R0, R1).encodeByte should be(Hex.MSB("e1000001"))
      }
    }
  }

}