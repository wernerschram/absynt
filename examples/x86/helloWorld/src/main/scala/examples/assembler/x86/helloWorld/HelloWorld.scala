package examples.assembler.x86.helloWorld

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}

import assembler.ListExtensions._
import assembler.output.Elf.{Architecture, Executable}
import assembler.sections.{Section, SectionType}
import assembler.x86.ProcessorMode
import assembler.x86.instructions._
import assembler.x86.operands.Register._
import assembler.{EncodedString, Label}

object HelloWorld extends App {
  createFile()

  def createFile(): Unit = {



    implicit val processorMode: ProcessorMode = ProcessorMode.Protected

    val entry: Label = "Entry"
    val hello: Label = "Text"

    val text: Section = Section(SectionType.Text, ".text",
      // use the write Syscall
      { implicit val label: Label = entry; Move(0x04, EAX) } ::
      Move(0x01, EBX) ::
      Move.forLabel(hello, ECX) ::
      Move(9, EDX) ::
      Interrupt(0x80.toByte) ::
      // use the _exit Syscall
      Move(0x01, EAX) ::
      Move(0x00, EBX) ::
      Interrupt(0x80.toByte) ::
      Nil, 0x08048000
    )

    val data: Section = Section(SectionType.Data, ".data",
    { implicit val label: Label = hello; EncodedString("Hi World\n") } ::
      Nil, 0x08049000
    )

    val path = Paths.get(System.getProperty("java.io.tmpdir"))
    val outputPath = path.resolve("x86HellowWorld-output")
    Files.createDirectories(outputPath)
    val outputFilePath = outputPath.resolve("test.elf")
    val rawFilePath = outputPath.resolve("test.raw")
    val out = new FileOutputStream(outputFilePath.toFile)
    val raw = new FileOutputStream(rawFilePath.toFile)

    val exec = Executable(Architecture.X86, text :: data :: Nil, entry)
    val finalSection = text.encodable(exec)
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
