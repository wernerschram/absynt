package examples.assembler.x86.helloWorld

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}

import assembler.Elf.{Architecture, Executable}
import assembler.{Resource, EncodedString, Label}
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

    val section: Section = Section(
      // use the write Syscall
      { implicit val label: Label = entry; Move(0x04, EAX) } ::
      Move(0x01, EBX) ::
      Move.forLabel(text, ECX) ::
      Move(12, EDX) ::
      Interrupt(0x80.toByte) ::
      // use the _exit Syscall
      Move(0x01, EAX) ::
      Move(0x00, EBX) ::
      Interrupt(0x80.toByte) ::
      { implicit val label: Label = text; EncodedString("Hello World!\r\n\u0000") } ::
      Nil, 0x1000
    )


    val path = Paths.get(System.getProperty("java.io.tmpdir"))
    val outputPath = path.resolve("x86HellowWorld-output")
    Files.createDirectories(outputPath)
    val outputFilePath = outputPath.resolve("test.elf")
    val rawFilePath = outputPath.resolve("test.raw")
    val out = new FileOutputStream(outputFilePath.toFile)
    val raw = new FileOutputStream(rawFilePath.toFile)

    val exec = Executable(Architecture.X86, section :: Nil, entry)
    val finalSection = section.encodable(exec)
    finalSection.finalContent.foreach { x => Console.println(s"${x.encodeByte.hexString} $x") }
    raw.write(finalSection.encodeByte.toArray)
    Console.println(s"output to file $outputFilePath")
    out.write(exec.encodeByte.toArray)
    raw.flush()
    out.flush()

    //objdump -b binary -D /tmp/x86HellowWorld-output/test.raw -m x86_64 -M intel
    //objdump -D /tmp/x86HellowWorld-output/test.elf -M intel
  }

}
