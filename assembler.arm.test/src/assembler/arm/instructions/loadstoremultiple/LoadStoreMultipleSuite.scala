package assembler.arm.instructions.dataprocessing

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec
import assembler.Hex
import assembler.MemoryPage
import assembler.arm.ProcessorMode
import assembler.arm.instructions.ARMInstruction
import assembler.arm.instructions.LoadMultiple
import assembler.arm.opcodes.UpdateMode
import assembler.arm.operands.registers.GeneralRegister._
import assembler.arm.instructions.StoreMultiple
import assembler.arm.instructions.ReturnFromException

class LoadStoreMultipleSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[ARMInstruction])

  "a LoadMultiple instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32
      "correctly encode ldm r4, {r1, r2}" in {
        LoadMultiple(R1 :: R2 :: Nil, R4, UpdateMode.IncrementAfter).encode should be(Hex.MSB("e8940006"))
      }
      
      "correctly encode ldm r1!, {r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14}" in {
        LoadMultiple.withUpdateBase(R1 :: R2 :: R3 :: R4 :: R5 :: R6 :: R7 :: R8 :: R9 :: R10 :: R11 :: R12 :: R13 :: R14 :: Nil, R1, UpdateMode.IncrementAfter).encode should be(Hex.MSB("e8b17ffe"))
      }

      "correctly encode ldm r1, {r14}^" in {
        LoadMultiple.withUserModeRegisters(R14 :: Nil, R1, UpdateMode.IncrementAfter).encode should be(Hex.MSB("e8d14000"))
      }
      
       "correctly encode ldm r1!, {r15, r14}^" in {
        LoadMultiple.withUserModeRegistersAndUpdateBase(R15 :: R14 :: Nil, R1, UpdateMode.IncrementAfter).encode should be(Hex.MSB("e8f1c000"))
      }

      "throw an AssertionError for ldm r1!, {r14}^" in {
        an[AssertionError] should be thrownBy {
          LoadMultiple.withUserModeRegistersAndUpdateBase(R14 :: Nil, R1, UpdateMode.IncrementAfter)
        }
      }
    }
  }
  
  "a StoreMultiple instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32
      "correctly encode stm r4, {r1, r2}" in {
        StoreMultiple(R1 :: R2 :: Nil, R4, UpdateMode.IncrementAfter).encode should be(Hex.MSB("e8840006"))
      }
      
      "correctly encode stm r1!, {r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14}" in {
        StoreMultiple.withUpdateBase(R1 :: R2 :: R3 :: R4 :: R5 :: R6 :: R7 :: R8 :: R9 :: R10 :: R11 :: R12 :: R13 :: R14 :: Nil, R1, UpdateMode.IncrementAfter).encode should be(Hex.MSB("e8a17ffe"))
      }

      "correctly encode stm r1, {r14}^" in {
        StoreMultiple.withUserModeRegisters(R14 :: Nil, R1, UpdateMode.DecrementBefore).encode should be(Hex.MSB("e9414000"))
      }
    }
  }
  
  "a ReturnFromException instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32
      "correctly encode rfeia r5" in {
        ReturnFromException(R5, UpdateMode.IncrementAfter).encode should be(Hex.MSB("f8950a00"))
      }
      
      "correctly encode rfedb r5!" in {
        ReturnFromException.withUpdateBase(R5, UpdateMode.DecrementBefore).encode should be(Hex.MSB("f9350a00"))
      }
    }
  }
}