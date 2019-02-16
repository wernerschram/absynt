package examples.assembler.x86.bootFlag

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}

import assembler.ListExtensions._
import assembler.output.raw.Raw
import assembler.resource.EncodableConversion._
import assembler.resource.Resource
import assembler.sections.Section
import assembler.x86.ProcessorMode
import assembler.x86.instructions._
import assembler.x86.operands.ByteSize
import assembler.x86.operands.Register._
import assembler.x86.operands.memoryaccess.DestinationReference
import assembler.{Label, UniqueLabel}

object Boot extends App {
  createFile()

  case class Color(r: Byte, g: Byte, b: Byte)

  import ProcessorMode.Real._

  def setColor(col: Color)(implicit processorMode: ProcessorMode): List[Resource] =
    Move(0x3c9.toShort, DX) ::
    Move(col.r, AL) ::
    Output(AL, DX) ::
    Move(col.g, AL) ::
    Output(AL, DX) ::
    Move(col.b, AL) ::
    Output(AL, DX) ::
    Nil

  def createFile(): Unit = {


    val topColor = Color(63, 0, 0)
    val middleColor = Color(63, 63, 63)
    val bottomColor = Color(0, 0, 63)

    val section: Section = Section.text(

      Move(0x13.toShort, AX) ::
      Interrupt(0x10.toByte) ::
      //
      Xor(AL, AL) ::
      Move(0x3c8.toShort, DX) ::
      Output(AL, DX) ::
      //
      setColor(topColor) :::
      setColor(middleColor) :::
      setColor(bottomColor) :::
      //
      Xor(DI, DI) ::
      Move(0xa000.toShort, AX) ::
      Move(AX, ES) ::
      //
      Move(0x0.toByte, AL) ::
      Move((320*67).toShort, CX) ::
      StoreString.Repeat(AL, DestinationReference[ByteSize](DI)) ::
      //
      Move(0x1.toByte, AL) ::
      Move((320*66).toShort, CX) ::
      StoreString.Repeat(AL, DestinationReference[ByteSize](DI)) ::
      //
      Move(0x2.toByte, AL) ::
      Move((320*67).toShort, CX) ::
      StoreString.Repeat(AL, DestinationReference[ByteSize](DI)) ::

      { val label: UniqueLabel = Label.unique; Jump(label).label(label) } ::
      Nil
    )

    val path = Paths.get(System.getProperty("java.io.tmpdir"))
    val outputPath = path.resolve("bootFlags-output")
    Files.createDirectories(outputPath)
    val outputFilePath = outputPath.resolve("test.com")
    val out = new FileOutputStream(outputFilePath.toFile)

    val executable = Raw(section, 0x100)
    section.content.encodables(executable.encodablesForDependencies(section.content.dependentResources))
      .foreach { x => Console.println(s"${x.encodeByte.hexString} $x") }
    out.write(executable.encodeByte.toArray)
    Console.println(s"output to file $outputFilePath")
    out.flush()

    //objdump -b binary -D /tmp/bootFlags-output/test.com -m i8086 -M intel
  }

}
