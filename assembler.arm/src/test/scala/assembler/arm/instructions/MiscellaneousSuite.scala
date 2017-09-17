package assembler.arm.instructions

import assembler.Hex
import assembler.arm.ProcessorMode
import assembler.arm.operations.{Effect, ExecutionMode, InterruptDisableFlags}
import org.scalatest.{Matchers, WordSpec}

class MiscellaneousSuite extends WordSpec with Matchers {

  "a Breakpoint instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode bkpt 0x00cc" in {
        Breakpoint(0xf15.toShort).encodeByte should be(Hex.msb("e120f175"))
      }

      "correctly represent bkpt 3861 as a string" in {
        Breakpoint(0xf15.toShort).toString should be("bkpt 3861")
      }
    }
  }

  "a Change Processor State instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode cpsie , #31" in {
        ChangeProcessorState(Effect.InterruptEnable, InterruptDisableFlags.none, ExecutionMode.System).encodeByte should be(Hex.msb("f10a001f"))
      }

      "correctly represent cpsie , #31 as a string" in {
        ChangeProcessorState(Effect.InterruptEnable, InterruptDisableFlags.none, ExecutionMode.System).toString should be("cpsie , #31")
      }

      "correctly encode cpsid ai, #16" in {
        ChangeProcessorState(Effect.InterruptDisable, InterruptDisableFlags.impreciseDataAbort + InterruptDisableFlags.normalInterrupt, ExecutionMode.User).encodeByte should be(Hex.msb("f10e0190"))
      }

      "correctly encode cpsie f" in {
        ChangeProcessorState(Effect.InterruptEnable, InterruptDisableFlags.fastInterrupt).encodeByte should be(Hex.msb("f10a0040"))
      }

      "correctly represent cpsie f as a string" in {
        ChangeProcessorState(Effect.InterruptEnable, InterruptDisableFlags.fastInterrupt).toString should be("cpsie f")
      }

      "correctly encode cps #19" in {
        ChangeProcessorState(ExecutionMode.Supervisor).encodeByte should be(Hex.msb("f1020013"))
      }
    }
  }
}