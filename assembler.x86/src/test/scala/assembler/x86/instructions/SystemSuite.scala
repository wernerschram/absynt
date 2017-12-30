package assembler.x86.instructions

import assembler.Hex
import assembler.x86.ProcessorMode
import assembler.x86.operands.ImmediateValue._
import org.scalatest.{Matchers, WordSpec}

class SystemSuite extends WordSpec with Matchers {

  "a SysCall instruction" when {

    "in real mode" should {
      import ProcessorMode.Real._

      "throw an AssertionError for syscall" in {
        an [AssertionError] should be thrownBy { SystemCall().encodeByte }
      }

      "throw an AssertionError for sysenter" in {
        an [AssertionError] should be thrownBy { SystemEnter().encodeByte }
      }

      "throw an AssertionError for sysret" in {
        an [AssertionError] should be thrownBy { SystemReturn(ProcessorMode.Protected).encodeByte }
      }

      "throw an AssertionError for sysexit" in {
        an [AssertionError] should be thrownBy { SystemExit(ProcessorMode.Protected).encodeByte }
      }
    }

    "in protected mode" should {
      import ProcessorMode.Protected._


      "throw an AssertionError for syscall" in {
        an [AssertionError] should be thrownBy { SystemCall().encodeByte }
      }

      "correctly encode sysenter" in {
        SystemEnter().encodeByte should be (Hex.lsb("0F 34"))
      }

      "correctly encode sysexit to protected mode" in {
        SystemExit(ProcessorMode.Protected).encodeByte should be (Hex.lsb("0F 35"))
      }

      "throw an AssertionError for sysexit to long mode" in {
        an [AssertionError] should be thrownBy { SystemExit(ProcessorMode.Long).encodeByte }
      }

      "throw an AssertionError for sysret to protected mode" in {
        an [AssertionError] should be thrownBy { SystemReturn(ProcessorMode.Protected).encodeByte }
      }

      "throw an AssertionError for sysret to long mode" in {
        an [AssertionError] should be thrownBy { SystemReturn(ProcessorMode.Long).encodeByte }
      }

    }

    "in long mode" should {

      import ProcessorMode.Long._

      "correctly encode syscall" in {
        SystemCall().encodeByte should be (Hex.lsb("0F 05"))
      }

      "correctly represent syscall as a string" in {
        SystemCall().toString should be("syscall")
      }

      "correctly encode sysenter" in {
        SystemEnter().encodeByte should be (Hex.lsb("0F 34"))
      }

      "correctly represent sysenter as a string" in {
        SystemEnter().toString should be("sysenter")
      }

      "correctly encode sysexit to protected mode" in {
        SystemExit(ProcessorMode.Protected).encodeByte should be (Hex.lsb("0F 35"))
      }

      "correctly encode sysexit to long mode" in {
        SystemExit(ProcessorMode.Long).encodeByte should be (Hex.lsb("48 0F 35"))
      }

      "correctly represent sysexit as a string" in {
        SystemExit(ProcessorMode.Protected).toString should be("sysexit")
      }

      "correctly encode sysret to protected mode" in {
        SystemReturn(ProcessorMode.Protected).encodeByte should be (Hex.lsb("0F 07"))
      }

      "correctly encode sysret to long mode" in {
        SystemReturn(ProcessorMode.Long).encodeByte should be (Hex.lsb("48 0F 07"))
      }

      "correctly represent sysret as a string" in {
        SystemReturn(ProcessorMode.Protected).toString should be("sysret")
      }

    }
  }
}