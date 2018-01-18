package examples.assembler.x86.helloWorld64

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}

import assembler.ListExtensions._
import assembler.output.Elf.{Architecture, Executable}
import assembler.resource.{AbsoluteReference, UnlabeledDependentResource, UnlabeledEncodable, RelativeReference}
import assembler.sections.{Section, SectionType}
import assembler.x86.ProcessorMode
import assembler.x86.instructions._
import assembler.x86.operands.Register._
import assembler.{EncodedString, Label}
import assembler.resource.EncodableConversion._

object HelloWorld extends App {
  createFile()

  def createFile(): Unit = {

    import ProcessorMode.Long._

    val entry: Label = "Entry"
    val hello: Label = "Text"

    val output: String = "Hello World!\n"

    val text: Section = Section(SectionType.Text, ".text",
      // use the write Syscall
      Move(0x01.toLong, RAX).label(entry) ::
      Move(0x01.toLong, RDI) ::
      Move.forLabel(hello, RSI) ::
      Move(output.length, EDX) ::
      SystemCall() ::
      // use the _exit Syscall
      Move(0x3C.toLong, RAX) ::
      Move(0x00.toLong, RDI) ::
      SystemCall() ::
      Nil, 16
    )

    val data: Section = Section(SectionType.Data, ".data",
      EncodedString(output).label(hello) ::
      Nil, 4
    )

    val path = Paths.get(System.getProperty("java.io.tmpdir"))
    val outputPath = path.resolve("x86HelloWorld64-output")
    Files.createDirectories(outputPath)
    val outputFilePath = outputPath.resolve("helloworld")
    val out = new FileOutputStream(outputFilePath.toFile)

    val exec = Executable(Architecture.X86_64, text :: data :: Nil, entry, 0x8048000)
    (text.content zip text.content.encodables(exec.encodablesForDependencies(text.content.dependentResources))).foreach {
      case (orig: RelativeReference, encoded) => Console.println(s"${encoded.encodeByte.hexString} $encoded (${orig.target})")
      case (orig: AbsoluteReference, encoded) => Console.println(s"${encoded.encodeByte.hexString} $encoded (${orig.target})")
      case (_, encoded) => Console.println(s"${encoded.encodeByte.hexString} $encoded")
    }
    Console.println(s"output to file $outputFilePath")
    out.write(exec.encodeByte.toArray)
    out.flush()

    //objdump -D /tmp/x86HelloWorld64-output/helloworld -M intel
    //readelf /tmp/x86HelloWorld64-output/helloworld -a
  }

}
