package examples.assembler.arm.BootRpi

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}

import assembler.Elf.{Architecture, Executable}
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

  private def naiveDelay(delay: Int, register: GeneralRegister)(implicit label: Label, processorMode: ProcessorMode): List[Resource] = {
    val targetLabel = Label.unique

    Move.forConstant(delay, register) ::
    { implicit val label: UniqueLabel = targetLabel; Subtract.setFlags(register, 1.toByte, register) } ::
    Branch(targetLabel, NotEqual) ::
    Nil
  }

  private def halt()(implicit label: Label, processorMode: ProcessorMode) = { implicit val label: UniqueLabel = Label.unique; Branch(label) }

  def createFile(): Unit = {
    implicit val processorMode: ProcessorMode = ProcessorMode.A32

    val putString: Label = "PutString"
    val text: Label = "Text"
    val entry: Label = "Entry"

    val section: Section = Section(SectionType.Text, ".text",
      // Disable UART0
      { implicit val label: Label = entry; Move.forConstant(UART0.Base, R0) } ::
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
      //
      // Divider = 3000000 / (16 * 115200) = 1.627 = ~1.
      // Fractional part register = (.627 * 64) + 0.5 = 40.6 = ~40.
      Move.forConstant(1, R1) ::
      StoreRegister(R1, R0, UART0.IBRD) ::
      //
      // Enable FIFO & 8 bit data transmissio (1 stop bit, no parity).
      Move.forConstant(40, R1) ::
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
      { implicit val label: Label = putString; LoadRegister(R4, R0, UART0.FR)} ::
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
      { implicit val label: Label = text; EncodedString("Hello World!\r\n\u0000") } :: Nil
    , 0x1000)

    val path = Paths.get(System.getProperty("java.io.tmpdir"))
    val outputPath = path.resolve("bootRpi-output")
    Files.createDirectories(outputPath)
    val outputFilePath = outputPath.resolve("test.elf")
    val rawFilePath = outputPath.resolve("test.raw")
    val out = new FileOutputStream(outputFilePath.toFile)
    val raw = new FileOutputStream(rawFilePath.toFile)

    val app = new Application(section :: Nil) {
      override def encodeByte: List[Byte] = ???
    }

    val finalSection = section.encodable(app)
    raw.write(finalSection.encodeByte.toArray)
    println(s"size: ${finalSection.size}")
    finalSection.finalContent.foreach { x => Console.println(s"${x.encodeByte.bigEndianHexString} $x") }

    val exec = Executable(Architecture.RaspberryPi2, section :: Nil, entry)
    out.write(exec.encodeByte.toArray)
   out.flush()
  }

  createFile()

  // To decompile the output download the gcc cross compiler for arm and execute:
  //  arm-linux-gnueabi-objdump -D /tmp/bootRpi-output/test.elf -m arm
  //
  // To run the example on qemu:
  // qemu-system-arm --kernel /tmp/bootRpi-output/test.raw -machine raspi2 -serial stdio
  //
  // architectures other that raspberry pi 2 could be supported by changing the GPIO
  // and UART base addresses
  //
}
