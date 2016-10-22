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

      "correctly encode adcs r3, r1, r14, lsl r5" in {
        AddCarry.setFlags(R1, Shifter.LogicalLeftShift(R14, R5), R3).encodeByte should be(Hex.msb("e0b1351e"))
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

      "correctly encode adc r4, r5, 0xffffffff" in {
        // Because a dataprocessing instruction cannot encode 0x1234, this instruction will be split up into two instructions as below:
        //        e2a540ff        adc     r4, r5, #255    ; 0xff
        //        e2844cff        add     r4, r4, #65280  ; 0xff00
        //        e28448ff        add     r4, r4, #16711680       ; 0xff0000
        //        e28444ff        add     r4, r4, #-16777216      ; 0xff000000
        AddCarry.forConstant(R5, 0xffffffff, R4).flatMap { instruction => instruction.encodeByte() } should be(Hex.msb("e2a540ff e2844cff e28448ff e28444ff"))
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

      "correctly encode add r4, r5, 0x0" in {
        Add.forConstant(R5, 0x0, R4).flatMap { instruction => instruction.encodeByte() } should be(Nil)
      }

      "correctly encode add r4, r5, 0x10011001" in {
        // Because a dataprocessing instruction cannot encode 0x1234, this instruction will be split up into two instructions as below:
        //        e2854001        add     r4, r5, #1
        //        e2844a11        add     r4, r4, #69632  ; 0x11000
        //        e2844201        add     r4, r4, #268435456      ; 0x10000000
        Add.forConstant(R5, 0x10011001, R4).flatMap { instruction => instruction.encodeByte() } should be(Hex.msb("e2854001 e2844a11 e2844201"))
      }

    }
  }

  "an And instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode and r2, r0, r1" in {
        And(R0, R1, R2).encodeByte should be(Hex.msb("e0002001"))
      }

      "correctly encode and r4, r5, 0x0" in {
        //        e2054000        and     r4, r5, #0
        And.forConstant(R5, 0x0, R4).flatMap { instruction => instruction.encodeByte() } should be(Hex.msb("e2054000"))
      }

      "correctly encode and r4, r5, 0x55555555" in {
        // Because a dataprocessing instruction cannot encode 0x55555555, this instruction will be split up into two instructions as below:
        //        e3c540aa        bic     r4, r5, #170    ; 0xaa
        //        e3c44caa        bic     r4, r4, #43520  ; 0xaa00
        //        e3c448aa        bic     r4, r4, #11141120       ; 0xaa0000
        //        e3c444aa        bic     r4, r4, #-1442840576    ; 0xaa000000
        And.forConstant(R5, 0x55555555, R4).flatMap { instruction => instruction.encodeByte() } should be(Hex.msb("e3c540aa e3c44caa e3c448aa e3c444aa"))
      }

      "correctly encode and r4, r5, 0xFFFF5555" in {
        // Because a dataprocessing instruction cannot encode 0x55555555, this instruction will be split up into two instructions as below:
        //        e3c540aa        bic     r4, r5, #170    ; 0xaa
        //        e3c44caa        bic     r4, r4, #43520  ; 0xaa00
        // Two superflous instructions are generated
        And.forConstant(R5, 0xFFFF5555, R4).flatMap { instruction => instruction.encodeByte() } should be(Hex.msb("e3c540aa e3c44caa"))
      }

      "correctly encode and r4, r5, 0x00005555" in {
        //        e3c540aa        bic     r4, r5, #170    ; 0xaa
        //        e3c44caa        bic     r4, r4, #43520  ; 0xaa00
        //        e3c448ff        bic     r4, r4, #16711680       ; 0xff0000
        //        e3c444ff        bic     r4, r4, #-16777216      ; 0xff000000
        And.forConstant(R5, 0x00005555, R4).flatMap { instruction => instruction.encodeByte() } should be(Hex.msb("e3c540aa e3c44caa e3c448ff e3c444ff"))
      }
    }
  }

  "an BitClear instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode and r2, r0, r1" in {
        BitClear(R0, R1, R2).encodeByte should be(Hex.msb("e1c02001"))
      }

      "correctly encode bic r4, r5, 0x0" in {
        BitClear.forConstant(R5, 0x0, R4).flatMap { instruction => instruction.encodeByte() } should be(Nil)
      }

      "correctly encode bic r4, r5, 0x00005555" in {
        //        e3c54055        bic     r4, r5, #85     ; 0x55
        //        e3c44c55        bic     r4, r4, #21760  ; 0x5500
        BitClear.forConstant(R5, 0x00005555, R4).flatMap { instruction => instruction.encodeByte() } should be(Hex.msb("e3c54055 e3c44c55"))
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
        //        e3a04602        mov     r4, #2097152    ; 0x200000
        //        e3844202        orr     r4, r4, #536870912      ; 0x20000000
        Move.forConstant(0x20200000, R4).flatMap { instruction => instruction.encodeByte() } should be(Hex.msb("e3a04602 e3844202"))
      }

      "correctly encode mov r4, 0x0 (using the forShifters syntax)" in {
        Move.forConstant(0x0, R4).flatMap { instruction => instruction.encodeByte() } should be(Hex.msb("e3a04000"))
      }
    }
  }

  "an ExclusiveOr instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode eor r4, r5, r9 lsl #2" in {
        ExclusiveOr(R5, Shifter.LogicalLeftShift(R9, 2.toByte), R4).encodeByte should be(Hex.msb("e0254109"))
      }

      "correctly encode eor r4, r5, 0x10000001" in {
        //        e2254001        eor     r4, r5, #1
        //        e2244201        eor     r4, r4, #268435456      ; 0x10000000
        ExclusiveOr.forConstant(R5, 0x10000001, R4).flatMap { instruction => instruction.encodeByte() } should be(Hex.msb("e2254001 e2244201"))
      }

      "correctly encode eor r4, r5, 0x0" in {
        ExclusiveOr.forConstant(R5, 0x0, R4).flatMap { instruction => instruction.encodeByte() } should be(Nil)
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

      "correctly encode orr r4, r5, 0x12345678" in {
        // Because a dataprocessing instruction cannot encode 0x1234, this instruction will be split up into two instructions as below:
        //    e3854f9e        orr     r4, r5, #632    ; 0x278
        //    e3844b15        orr     r4, r4, #21504  ; 0x5400
        //    e384478d        orr     r4, r4, #36962304       ; 0x2340000
        //    e3844201        orr     r4, r4, #268435456      ; 0x10000000
        Or.forConstant(R5, 0x12345678, R4).flatMap { instruction => instruction.encodeByte() } should be(Hex.msb("e3854f9e e3844b15 e384478d e3844201"))
      }

      "correctly encode orr r4, r5, 0x0" in {
        Or.forConstant(R5, 0x0, R4).flatMap { instruction => instruction.encodeByte() } should be(Nil)
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

      "correctly encode sbc r4, r5, 0xF0F0F0F0" in {
        //        e2c54e0f        sbc     r4, r5, #15, 28 ; 0xf0
        //        e2444a0f        sub     r4, r4, #61440  ; 0xf000
        //        e244420f        sub     r4, r4, #-268435456     ; 0xf0000000
        SubtractCarry.forConstant(R5, 0xF000F0F0, R4).flatMap { instruction => instruction.encodeByte() } should be(Hex.msb("e2c54e0f e2444a0f e244420f"))
      }

      "correctly encode sbc r4, r5, 0x0" in {
        //        e2c54000        sbc     r4, r5, #0
        SubtractCarry.forConstant(R5, 0, R4).flatMap { instruction => instruction.encodeByte() } should be(Hex.msb("e2c54000"))
      }
    }
  }

  "a Subtract instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode sub r2, r0, r1" in {
        Subtract(R0, R1, R2).encodeByte should be(Hex.msb("e0402001"))
      }

      "correctly encode sub r4, r5, 0x0" in {
        Subtract.forConstant(R5, 0x0, R4).flatMap { instruction => instruction.encodeByte() } should be(Nil)
      }

      "correctly encode sub r4, r5, 0x88888888" in {
        //        e2454f22        sub     r4, r5, #34, 30 ; 0x88
        //        e2444b22        sub     r4, r4, #34816  ; 0x8800
        //        e2444722        sub     r4, r4, #8912896        ; 0x880000
        //        e2444322        sub     r4, r4, #-2013265920    ; 0x88000000
        Subtract.forConstant(R5, 0x88888888, R4).flatMap { instruction => instruction.encodeByte() } should be(Hex.msb("e2454f22 e2444b22 e2444722 e2444322"))
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