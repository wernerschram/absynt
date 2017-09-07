package examples.assembler.x86.helloWorld

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}

import assembler.Elf.{Architecture, Executable, HasName}
import assembler.{Encodable, EncodedString, Label}
import assembler.ListExtensions._
import assembler.sections.Section
import assembler.x86.ProcessorMode
import assembler.x86.instructions._
import assembler.x86.operands.Register._

object HelloWorld extends App {
  createFile()

  def createFile(): Unit = {

    implicit val processorMode: ProcessorMode = ProcessorMode.Protected

    val entry: Label = "Entry"
    val text: Label = "Text"

    val page: Section = Section(
      { implicit val label = entry; Move(0x04, EAX) } ::
      // use the write Syscall
      Move(0x01, EBX) ::
      Move(0x01, ECX) ::
      Move(0x12, EDX) ::
      Interrupt(0x80.toByte) ::
      // use the _exit Syscall
      Move(0x01, EAX) ::
      Move(0x00, EBX) ::
      Interrupt(0x80.toByte) ::
      { implicit val label = text; EncodedString("Hello World!\r\n\u0000") } ::
      Nil, 0
    )

    val path = Paths.get(System.getProperty("java.io.tmpdir"))
    val outputPath = path.resolve("x86HellowWorld-output")
    Files.createDirectories(outputPath)
    val outputFilePath = outputPath.resolve("test.elf")
    val rawFilePath = outputPath.resolve("test.raw")
    val out = new FileOutputStream(outputFilePath.toFile)
    val raw = new FileOutputStream(rawFilePath.toFile)

    implicit object nameProvider extends HasName[Section] {
      override def name(x: Section): String = ".text"
    }
    page.content.collect { case x: Encodable => x }.foreach { x => Console.println(s"${x.encodeByte()(page).hexString} $x") }
    raw.write(page.encodeByte().toArray)
    Console.println(s"output to file $outputFilePath")
    val exec = Executable(Architecture.X86, page :: Nil, entry)
    out.write(exec.header.toArray)
    raw.flush()
    out.flush()

    //objdump -b binary -D /tmp/x86HellowWorld-output/test.raw -m x86_64 -M intel
    //objdump -D /tmp/x86HellowWorld-output/test.elf
  }

}
