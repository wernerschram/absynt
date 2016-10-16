package assembler.arm.instructions.branch

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.arm.ProcessorMode
import assembler.arm.instructions.ARMInstruction
import assembler.arm.instructions.dataprocessing.MoveFromStatusRegister
import assembler.arm.instructions.dataprocessing.MoveToStatusRegister
import assembler.arm.opcodes.Fields
import assembler.arm.operands.Condition
import assembler.arm.operands.registers.GeneralRegister._
import assembler.arm.operands.registers.StatusRegister._
import assembler.memory.MemoryPage

class MoveFromStatusRegisterSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[ARMInstruction])

  "an MoveFromStatusRegister instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode mrs r1, CPSR" in {
        MoveFromStatusRegister(CPSR, R1).encodeByte should be(Hex.msb("e10f1000"))
      }

      "correctly encode mrscc r1, SPSR" in {
        MoveFromStatusRegister(SPSR, R1, Condition.UnsignedLower).encodeByte should be(Hex.msb("314f1000"))
      }
    }
  }

  "an MoveToStatusRegister instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode msr SPSR_xc, r1" in {
        MoveToStatusRegister(R1, SPSR, Fields.control + Fields.extension).encodeByte should be(Hex.msb("e163f001"))
      }

      "correctly encode msr CPSR_fsxc, r9" in {
        MoveToStatusRegister(R9, CPSR, Fields.extension + Fields.control + Fields.status + Fields.flags).encodeByte should be(Hex.msb("e12ff009"))
      }

      "correctly encode msr CPSR_fx, #240, 4" in {
        MoveToStatusRegister(0xf0.toByte, 4.toByte, CPSR, Fields.extension + Fields.flags).encodeByte should be(Hex.msb("e32af2f0"))
      }

      "correctly encode msr CPSR_fsxc, #16777216" in {
        MoveToStatusRegister(1.toByte, 8.toByte, CPSR, Fields.extension + Fields.control + Fields.status + Fields.flags).encodeByte should be(Hex.msb("e32ff401"))
      }

      "correctly encode msrls CPSR_fsxc, #256" in {
        MoveToStatusRegister(1.toByte, 24.toByte, CPSR, Fields.extension + Fields.control + Fields.status + Fields.flags, Condition.LowerOrSame).encodeByte should be(Hex.msb("932ffc01"))
      }
    }
  }
}

