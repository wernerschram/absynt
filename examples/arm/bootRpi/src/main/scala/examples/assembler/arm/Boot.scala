package examples.assembler.arm

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}

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


  object elf {
    def magic: List[Byte] = 0x7F.toByte :: Nil ::: "ELF".toCharArray.map(_.toByte).toList
    def `class`: List[Byte] = 0x01.toByte :: Nil // 32bit
    def data: List[Byte] = 0x01.toByte :: Nil // big endian
    def version: List[Byte] = 0x01.toByte :: Nil
    def osAbi: List[Byte] = 0x00.toByte :: Nil
    def abiVersion: List[Byte] = 0x00.toByte :: Nil
    def eiPad: List[Byte] = List.fill(7)(0x00.toByte)
    def eType: List[Byte] = 0x02.toByte :: 0x00.toByte :: Nil // ???
    def eMachine: List[Byte] = 0x89.toByte :: 0x00.toByte :: Nil // ARM
    def eVersion: List[Byte] = 0x01.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
    def entry: List[Byte] = List.fill(1)(0x00.toByte) ::: 0x02.toByte :: 0xc0.toByte :: 0xce.toByte :: Nil
    def phoff: List[Byte] = 0x34.toByte :: Nil ::: List.fill(3)(0x00.toByte)
    def shoff: List[Byte] = 0x54.toByte :: Nil ::: List.fill(3)(0x00.toByte)
    def flags: List[Byte] = 0x00.toByte :: 0x01.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
    def ehSize: List[Byte] = 0x34.toByte :: 0x00.toByte :: Nil
    def phEntSize: List[Byte] = 0x20.toByte :: 0x00.toByte :: Nil
    def phNum: List[Byte] = 0x01.toByte :: 0x00.toByte :: Nil
    def shEntSize: List[Byte] = 0x28.toByte :: 0x00.toByte :: Nil
    def shNum: List[Byte] = 0x03.toByte :: 0x00.toByte :: Nil
    def shstrndx: List[Byte] = 0x02.toByte :: 0x00.toByte :: Nil

    object programHeader {
      def `type`: List[Byte] = 0x01.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def offset: List[Byte] = 0xcc.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def vaddr: List[Byte] = 0x00.toByte :: 0x1e.toByte :: 0xc0.toByte :: 0xce.toByte :: Nil
      def paddr: List[Byte] = 0x00.toByte :: 0x1e.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def filesz: List[Byte] = 0xb0.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def memsz: List[Byte] = 0xb0.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def flags: List[Byte] = 0x05.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def allign: List[Byte] = 0x40.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil

      def header: List[Byte] =
        `type` :::
        offset :::
        vaddr :::
        paddr :::
        filesz :::
        memsz :::
        flags :::
        allign
    }

    object nullSection {
      def header: List[Byte] = List.fill(40)(0x00.toByte)
    }

    object textSectionHeader {
      def name: List[Byte] = 0x01.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def `type`: List[Byte] = 0x01.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def flags: List[Byte] = 0x06.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def addr: List[Byte] = 0x00.toByte :: 0x1e.toByte :: 0xc0.toByte :: 0xce.toByte :: Nil
      def offset: List[Byte] = 0xcc.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def filesz: List[Byte] = 0xb0.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def link: List[Byte] = 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def info: List[Byte] = 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def allign: List[Byte] = 0x40.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def entSize: List[Byte] = 0x01.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil

      def header: List[Byte] =
        name :::
        `type` :::
        flags :::
        addr :::
        offset :::
        filesz :::
        link :::
        info :::
        allign :::
        entSize
    }

    object stringSectionHeader {
      def name: List[Byte] = 0x07.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def `type`: List[Byte] = 0x03.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def flags: List[Byte] = 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def addr: List[Byte] = 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def offset: List[Byte] = 0x7c.toByte :: 0x01.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def filesz: List[Byte] = 0x11.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def link: List[Byte] = 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def info: List[Byte] = 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def allign: List[Byte] = 0x01.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil
      def entSize: List[Byte] = 0x01.toByte :: 0x00.toByte :: 0x00.toByte :: 0x00.toByte :: Nil

      def header: List[Byte] =
        name :::
        `type` :::
        flags :::
        addr :::
        offset :::
        filesz :::
        link :::
        info :::
        allign :::
        entSize
    }

    def header: List[Byte] =
      magic :::
      `class` :::
      data :::
      version :::
      osAbi :::
      abiVersion :::
      eiPad :::
      eType :::
      eMachine :::
      eVersion :::
      entry :::
      phoff :::
      shoff :::
      flags:::
      ehSize :::
      phEntSize :::
      phNum :::
      shEntSize :::
      shNum :::
      shstrndx :::
      programHeader.header ::: //0x34
      nullSection.header ::: // 0x54
      textSectionHeader.header ::: //0x7c
      stringSectionHeader.header //0xa4
  }


  private def naiveDelay(delay: Int, register: GeneralRegister)(implicit label: Label, processorMode: ProcessorMode): List[Encodable] = {
    val targetLabel = Label.unique

    Move.forConstant(delay, register) ::
    List[Encodable](
      { implicit val label = targetLabel; Subtract.setFlags(register, 1.toByte, register) },
      Branch(targetLabel, NotEqual)
    )
  }

  private def halt()(implicit label: Label, processorMode: ProcessorMode) = { implicit val label = Label.unique; Branch(label) }

  def createFile(): Unit = {
    implicit val processorMode = ProcessorMode.A32

    val putString: Label = "PutString"
    val text: Label = "Text"

    val page: Section = Section(
      // Disable UART0
      Move.forConstant(UART0.Base, R0) ::
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
      { implicit val label = putString; LoadRegister(R4, R0, UART0.FR)} ::
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
      { implicit val label = text; EncodedString("Hello World!\r\n\0") } :: Nil
    )

    val path = Paths.get(System.getProperty("java.io.tmpdir"))
    val outputPath = path.resolve("bootRpi-output")
    Files.createDirectories(outputPath)
    val outputFilePath = outputPath.resolve("test.elf")
    val rawFilePath = outputPath.resolve("test.raw")
    val out = new FileOutputStream(outputFilePath.toFile)
    val raw = new FileOutputStream(rawFilePath.toFile)
    raw.write(page.encodeByte()(page).toArray)
    println(s"size: ${page.size}")
    page.content.foreach { x => Console.println(s"${x.encodeByte()(page).bigEndianHexString} $x") }
    out.write((
      elf.header :::
        page.encodeByte()(page) :::
        0x00.toByte :: Nil :::
        ".text".toCharArray.map(_.toByte).toList ::: 0x00.toByte :: Nil :::
        ".shstrtab".toCharArray.map(_.toByte).toList ::: 0x00.toByte :: Nil
      ).toArray)
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
