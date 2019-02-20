package examples.assembler.x86.helloWorld32

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}

import assembler.ListExtensions._
import assembler.output.Elf.{Architecture, Executable}
import assembler.resource.EncodableConversion._
import assembler.resource.{AbsoluteReference, RelativeReference}
import assembler.sections.Section
import assembler.x86.ProcessorMode
import assembler.x86.instructions._
import assembler.{EncodedString, Label}

object HelloWorld extends App {
  createFile()

  def createFile(): Unit = {

    import ProcessorMode.Protected._

    val entry: Label = "Entry"
    val hello: Label = "Text"

    val output: String = "Hello World!\n"

    val text: Section = Section.text(
      // use the write Syscall
      Move(0x04, EAX).label(entry) ::
      Move(0x01, EBX) ::
      Move.forLabel(hello, ECX) ::
      Move(output.length, EDX) ::
      Interrupt(0x80.toByte) ::
      // use the _exit Syscall
      Move(0x01, EAX) ::
      Move(0x00, EBX) ::
      Interrupt(0x80.toByte) ::
      Nil
    )

    val data: Section = Section.data(
      EncodedString(output).label(hello) ::
      Nil, alignment = 4
    )

    val path = Paths.get(System.getProperty("java.io.tmpdir"))
    val outputPath = path.resolve("x86HelloWorld32-output")
    Files.createDirectories(outputPath)
    val outputFilePath = outputPath.resolve("helloworld")
    val out = new FileOutputStream(outputFilePath.toFile)

    val exec = Executable(Architecture.X86, text :: data :: Nil, entry, 0x8048000)
    (text.content zip text.content.encodables(exec.encodablesForDependencies(text.content.dependentResources))).foreach {
      case (orig: RelativeReference, encoded) => Console.println(s"${encoded.encodeByte.hexString} $encoded (${orig.target})")
      case (orig: AbsoluteReference, encoded) => Console.println(s"${encoded.encodeByte.hexString} $encoded (${orig.target})")
      case (_, encoded) => Console.println(s"${encoded.encodeByte.hexString} $encoded")
    }
    Console.println(s"output to file $outputFilePath")
    out.write(exec.encodeByte.toArray)
    out.flush()

    //objdump -D /tmp/x86HelloWorld32-output/helloworld -M intel
    //readelf /tmp/x86HelloWorld32-output/helloworld -a
  }

}
