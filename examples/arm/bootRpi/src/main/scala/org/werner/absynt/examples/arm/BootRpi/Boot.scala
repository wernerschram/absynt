/*
 * Copyright 2019 Werner Schram
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

package org.werner.absynt.examples.arm.BootRpi

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}

import org.werner.absynt._
import org.werner.absynt.arm.ProcessorMode
import org.werner.absynt.arm.operands.registers.GeneralRegister
import org.werner.absynt.output.Elf.{Architecture, Executable}
import org.werner.absynt.output.raw.Raw
import org.werner.absynt.resource.Resource
import org.werner.absynt.sections.Section
import scala.language.implicitConversions

object Boot extends App {

  import ProcessorMode.A32.{given, *}

  object GPIO {
    val Base = 0x3F200000

    val GPPUD: Short = 0x94.toShort
    val GPPUDCLK0: Short = 0x98.toShort
  }

  object UART0 {
    val Base: Int = GPIO.Base + 0x1000

    val DR: Short = 0x00
    val RSRECR: Short = 0x04
    val FR: Short = 0x18
    val ILPR: Short = 0x20
    val IBRD: Short = 0x24
    val FBRD: Short = 0x28
    val LCRH: Short = 0x2C.toShort
    val CR: Short = 0x30
    val IFLS: Short = 0x34
    val IMSC: Short = 0x38
    val RIS: Short = 0x3C.toShort
    val MIS: Short = 0x40
    val ICR: Short = 0x44
    val DMACR: Short = 0x48
    val ITCR: Short = 0x80
    val ITIP: Short = 0x84
    val ITOP: Short = 0x88
    val TDR: Short = 0x8C.toShort
  }

  private def naiveDelay(delay: Int, register: GeneralRegister): List[Resource] = {
    val targetLabel = Label.unique

    Move.forConstant(delay, register) ::
    Subtract.setFlags(register, 1.toByte, register).label(targetLabel) ::
    Branch(targetLabel, NotEqual) ::
    Nil
  }

  private def halt() = {
    val label = Label.unique
    Branch(label).label(label)
  }

  def createFile(): Unit = {
    import ProcessorMode.A32.{given, *}

    val putString: Label = "PutString"
    val text: Label = "Text"
    val entry: Label = "Entry"

    val uartClock = 3000000
    val baudRate = 115200
    val divider = uartClock.toDouble / (16 * baudRate)

    val dividerInt = divider.toInt
    val dividerFraction = divider - dividerInt.toDouble
    val dividerFractionRegister = ((dividerFraction * 64) + 0.5).toInt

    println(s"divider int: $dividerInt")
    println(s"divider fraction register: $dividerFractionRegister")

    val section: Section = Section.text(

      // Disable UART0
      Move.forConstant(UART0.Base, R0).label(entry) ::
      Move.forConstant(0, R1) ::
      StoreRegister(R1, R0, UART0.CR) ::
      //
      // Disable pull up/down for all GPIO pins & delay for 150 cycles.
      Move.forConstant(GPIO.Base, R2) ::
      StoreRegister(R1, R2, GPIO.GPPUD) ::
      naiveDelay(150, R4) :::
      //
      // Disable pull up/down for pin 14,15 & delay for 150 cycles.
      Move.forConstant(3 << 14, R3) ::
      StoreRegister(R3, R2, GPIO.GPPUDCLK0) ::
      naiveDelay(150, R4) :::
      //
      // Write 0 to GPPUDCLK0 to make it take effect.
      StoreRegister(R1, R2, GPIO.GPPUDCLK0) ::
      // Clear pending interrupts.
      Move.forConstant(0x7FF, R1) ::
      StoreRegister(R1, R0, UART0.ICR) ::
      //
      // Set integer & fractional part of baud rate.
      // Divider = UART_CLOCK/(16 * Baud)
      // Fraction part register = (Fractional part * 64) + 0.5
      // UART_CLOCK = 3000000; Baud = 115200.
      Move.forConstant(dividerInt, R1) ::
      StoreRegister(R1, R0, UART0.IBRD) ::
      //
      // Enable FIFO & 8 bit data transmissio (1 stop bit, no parity).
      Move.forConstant(dividerFractionRegister, R1) ::
      StoreRegister(R1, R0, UART0.FBRD) ::
      //
      // Mask all interrupts.
      Move.forConstant(0x70, R1) ::
      StoreRegister(R1, R0, UART0.LCRH) ::
      //
      Move.forConstant(0x7F2, R1) ::
      StoreRegister(R1, R0, UART0.IMSC) ::
      //
      // Enable UART0, receive & transfer part of UART.
      Move.forConstant(0x301, R1) ::
      StoreRegister(R1, R0, UART0.CR) ::
      //
      // Put the string from label [text] on the serial line
      Add.forRelativeLabel(PC, text, R7) ::
      Move.forConstant(0.toByte, R6) ::
      //
      LoadRegister(R4, R0, UART0.FR).label(putString) ::
      And.setFlags(R4, 0x20.toByte, R4) ::
      Branch(putString, ZeroClear) ::
      //
      LoadRegister.byte(R5, R7, R6) ::
      StoreRegister.byte(R5, R0, UART0.DR) ::
      Add(R6, 1.toByte, R6) ::
      Compare(R6, 15.toByte) ::
      Branch(putString, NotEqual) ::
      //
      // Goal achieved.
      halt() ::
      //
      // Resources
      EncodedString("Hello World!\r\n\u0000").label(text) ::
      Nil)

    val path = Paths.get(System.getProperty("java.io.tmpdir"))
    val outputPath = path.resolve("bootRpi-output")
    Files.createDirectories(outputPath)
    val outputFilePath = outputPath.resolve("test.elf")
    val rawFilePath = outputPath.resolve("test.raw")
    val out = new FileOutputStream(outputFilePath.toFile)
    val raw = new FileOutputStream(rawFilePath.toFile)

    val exec = Executable(Architecture.RaspberryPi2, section :: Nil, entry, 0x10000)
    val rawExec = Raw(section, 0x10000)
//
//    raw.write(exec.encodableSections.head.encodeByte.toArray)
//    println(s"size: ${exec.encodableSections.head.size}")
//    exec.encodableSections.head.finalContent.foreach { x => Console.println(s"${x.encodeByte.bigEndianHexString} $x") }

    out.write(exec.encodeByte.toArray)
    out.flush()

    raw.write(rawExec.encodeByte.toArray)
    raw.flush()
  }

  createFile()

  // To decompile the output download the gcc cross compiler for arm and execute:
  //  arm-linux-gnueabi-objdump -b binary -D /tmp/bootRpi-output/test.raw -m arm
  //  arm-linux-gnueabi-objdump -D /tmp/bootRpi-output/test.elf -m arm
  //
  // To run the example on qemu:
  // qemu-system-arm --kernel /tmp/bootRpi-output/test.raw -machine raspi2 -serial stdio
  //
  // architectures other that raspberry pi 2 could be supported by changing the GPIO
  // and UART base addresses
  //
}
