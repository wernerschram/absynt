package examples.assembler.arm

import java.io.FileOutputStream

import assembler._
import assembler.ListExtensions._
import assembler.arm.ProcessorMode
import assembler.arm.instructions._
import assembler.arm.operands.Condition._
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operands.registers.GeneralRegister._
import assembler.sections.Section

object Boot extends App {

  object GPIO {
    val Base = 0x20200000

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
    val LCRH: Short = 0x2C
    val CR: Short = 0x30
    val IFLS: Short = 0x34
    val IMSC: Short = 0x38
    val RIS: Short = 0x3C
    val MIS: Short = 0x40
    val ICR: Short = 0x44
    val DMACR: Short = 0x48
    val ITCR: Short = 0x80
    val ITIP: Short = 0x84
    val ITOP: Short = 0x88
    val TDR: Short = 0x8C
  }

  createFile()

  def Labeled(label: Label, encodable:(Label) => Encodable): Unit = {

  }

  def x(a: Int=  5)(b: Int) = a + b
  def y(a: Int)(b: Int=  5) = a + b

  private def naiveDelay(delay: Int, register: GeneralRegister)(implicit label: Label, processorMode: ProcessorMode): List[Encodable] = {
    val x1: Int = x(1)(2)
    val x2: Int = x()(2)

    val y1: Int = y(1)(2)
    val y2: Int = y(1)()

    val targetLabel = Label.unique

    Move.forConstant(delay, register) :::
    List[Encodable](
      { implicit val label = targetLabel; Subtract.setFlags(register, 2.toByte, register) },
      Branch(targetLabel, NotEqual)
    )
  }

  def createFile(): Unit = {
    implicit val processorMode = ProcessorMode.A32

    val loop: Label = Label.unique
    val putString: Label = Label.unique
    val text: Label = Label.unique

    val page: Section = Section(
      // Disable UART0
      Move.forConstant(UART0.Base, R0) :::
      Move.forConstant(0, R1) :::
      StoreRegister(R1, R0, UART0.CR) ::
      //
      // Disable pull up/down for all GPIO pins & delay for 150 cycles.
      Move.forConstant(GPIO.Base, R2) :::
      //
      StoreRegister(R1, R2, GPIO.GPPUD) ::
      naiveDelay(150, R1) :::
      //
      // Disable pull up/down for pin 14,15 & delay for 150 cycles.
      Move.forConstant(3 << 14, R3) :::
      StoreRegister(R3, R2, GPIO.GPPUDCLK0) ::
      naiveDelay(150, R1) :::
      //
      // Write 0 to GPPUDCLK0 to make it take effect.
      StoreRegister(R1, R2, GPIO.GPPUDCLK0) ::
      // Clear pending interrupts.
      Move.forConstant(0x7FF, R1) :::
      StoreRegister(R1, R0, UART0.ICR) ::
      //
      // Set integer & fractional part of baud rate.
      // Divider = UART_CLOCK/(16 * Baud)
      // Fraction part register = (Fractional part * 64) + 0.5
      // UART_CLOCK = 3000000; Baud = 115200.
      //
      // Divider = 3000000 / (16 * 115200) = 1.627 = ~1.
      // Fractional part register = (.627 * 64) + 0.5 = 40.6 = ~40.
      Move.forConstant(1, R1) :::
      StoreRegister(R1, R0, UART0.IBRD) ::
      //
      // Enable FIFO & 8 bit data transmissio (1 stop bit, no parity).
      Move.forConstant(40, R1) :::
      StoreRegister(R1, R0, UART0.FBRD) ::
      //
      // Mask all interrupts.
      Move.forConstant(0x70, R1) :::
      StoreRegister(R1, R0, UART0.LCRH) ::
      Move.forConstant(0x7F1, R1) :::
      StoreRegister(R1, R0, UART0.IMSC) ::
      //
      // Enable UART0, receive & transfer part of UART.
      Move.forConstant(0x7F1, R1) :::
      StoreRegister(R1, R0, UART0.CR) ::
      Move(0.toByte, R6) ::
      { implicit val label = putString; LoadRegister(R4, R0, UART0.FR)} ::
      Compare(R4, 0x20.toByte) ::
      Branch(putString, NotEqual) ::
      //
      // TODO: get R6th character from string into R5
      LoadRegister(text, R5) ::
      StoreRegister(R5, R0, UART0.CR) ::
      Add(R6, 1.toByte, R6) ::
      Compare(R6, 12.toByte) ::
      Branch(putString, NotEqual) ::
      { implicit val label = text; EncodedString("Hello World!") } :: Nil

    )

    val out = new FileOutputStream("c:\\temp\\test.arm")
    page.content.collect { case x: Encodable => x }.foreach { x => Console.println(s"${x.encodeByte()(page).bigEndianHexString} $x") }
    out.write(page.encodeByte()(page).toArray)
    out.flush()
    //arm-eabi-objdump -b binary -marm -D test.arm
  }

}
