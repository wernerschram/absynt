package org.werner.absynt.x86.examples.helloWorld64

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}

import org.werner.absynt.ListExtensions._
import org.werner.absynt.output.Elf.{Architecture, Executable}
import org.werner.absynt.resource.EncodableConversion._
import org.werner.absynt.resource.{AbsoluteReference, RelativeReference}
import org.werner.absynt.sections.Section
import org.werner.absynt.x86.ProcessorMode
import org.werner.absynt.x86.operands.{ByteSize, DoubleWordSize}
import org.werner.absynt.{EncodedString, Label}

object HelloWorld extends App {
  createFile()

  def createFile(): Unit = {

    import ProcessorMode.Long._

    val entry: Label = "Entry"
    val hello: Label = "Text"

    val output: String = "Hello World!\n"

    val text: Section = Section.text(
      // use the write Syscall
      Move(0x01.toLong, RAX).label(entry) ::
      Move(0x01.toLong, RDI) ::
      Move.forLabel(hello, RSI) ::
      Move[DoubleWordSize](output.length, EDX) ::
      SystemCall() ::
      // use the _exit Syscall
      Move(0x3C.toLong, RAX) ::
      Move(0x00.toLong, RDI) ::
      SystemCall() ::
      Nil
    )

    val text2: Section = Section.text(
      Move(EBP, SIBMemoryLocation[DoubleWordSize](R9, R8, 0, 2)) ::
      Nil
    )

    val data: Section = Section.data(
      EncodedString(output).label(hello) ::
      Nil, alignment = 4
    )

    val path = Paths.get(System.getProperty("java.io.tmpdir"))
    val outputPath = path.resolve("x86HelloWorld64-output")
    Files.createDirectories(outputPath)
    val outputFilePath = outputPath.resolve("helloworld")
    val out = new FileOutputStream(outputFilePath.toFile)

    val exec = Executable(Architecture.X86_64, text :: text2 :: data :: Nil, entry, 0x8048000)
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
