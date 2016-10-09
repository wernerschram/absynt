package assembler.arm.instructions

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.arm.ProcessorMode
import assembler.memory.MemoryPage
import assembler.arm.opcodes.ExecutionMode
import assembler.arm.opcodes.Effect
import assembler.arm.opcodes.InterruptDisableFlags

class MiscellaneousSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[ARMInstruction])

  "a Breakpoint instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode bkpt 0x00cc" in {
        Breakpoint(0xf15.toShort).encodeByte should be(Hex.msb("e120f175"))
      }

    }
  }

  "a Change Processor State instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode cpsie,#31" in {
        ChangeProcessorState(Effect.InterruptEnable, InterruptDisableFlags.none, ExecutionMode.System).encodeByte should be(Hex.msb("f10a001f"))
      }

      "correctly encode cpsid ai,#16" in {
        ChangeProcessorState(Effect.InterruptDisable, InterruptDisableFlags.impreciseDataAbort + InterruptDisableFlags.normalInterrupt, ExecutionMode.User).encodeByte should be(Hex.msb("f10e0190"))
      }

    }
  }
}