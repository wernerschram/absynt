package assembler.x86.instructions

import assembler.Hex
import assembler.x86.ProcessorMode
import assembler.x86.operands.ImmediateValue._
import assembler.x86.operands.ReturnMode
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
        an [AssertionError] should be thrownBy { SystemReturn(ReturnMode.Protected).encodeByte }
      }

      "throw an AssertionError for sysexit" in {
        an [AssertionError] should be thrownBy { SystemExit(ReturnMode.Protected).encodeByte }
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
        SystemExit(ReturnMode.Protected).encodeByte should be (Hex.lsb("0F 35"))
      }

      "throw an AssertionError for sysexit to long mode" in {
        an [AssertionError] should be thrownBy { SystemExit(ReturnMode.Long).encodeByte }
      }

      "throw an AssertionError for sysret to protected mode" in {
        an [AssertionError] should be thrownBy { SystemReturn(ReturnMode.Protected).encodeByte }
      }

      "throw an AssertionError for sysret to long mode" in {
        an [AssertionError] should be thrownBy { SystemReturn(ReturnMode.Long).encodeByte }
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
        SystemExit(ReturnMode.Protected).encodeByte should be (Hex.lsb("0F 35"))
      }

      "correctly encode sysexit to long mode" in {
        SystemExit(ReturnMode.Long).encodeByte should be (Hex.lsb("48 0F 35"))
      }

      "correctly represent sysexit as a string" in {
        SystemExit(ReturnMode.Protected).toString should be("sysexit ")
      }

      "correctly encode sysret to protected mode" in {
        SystemReturn(ReturnMode.Protected).encodeByte should be (Hex.lsb("0F 07"))
      }

      "correctly encode sysret to long mode" in {
        SystemReturn(ReturnMode.Long).encodeByte should be (Hex.lsb("48 0F 07"))
      }

      "correctly represent sysret as a string" in {
        SystemReturn(ReturnMode.Protected).toString should be("sysret ")
      }
    }
  }
}