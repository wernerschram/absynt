package examples.assembler.x86.bootFlag

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}

import assembler.ListExtensions._
import assembler.output.raw.Raw
import assembler.sections.{Section, SectionType}
import assembler.x86.ProcessorMode
import assembler.x86.instructions._
import assembler.x86.operands.Register._
import assembler.x86.operands.memoryaccess.RealX86Offset
import assembler.{Label, Resource, UniqueLabel}

object Boot extends App {
  createFile()

  case class Color(r: Byte, g: Byte, b: Byte)

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

    import ProcessorMode.Real._

    val topColor = Color(63, 0, 0)
    val middleColor = Color(63, 63, 63)
    val bottomColor = Color(0, 0, 63)

    val targetLabel = Label.unique
    val section: Section = Section(SectionType.Text, ".text",


      JumpIfCountZero(targetLabel) ::
        Interrupt(0x03) ::
      { implicit val label: UniqueLabel = targetLabel; Interrupt(0x04.toByte)} ::



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
      StoreString.Repeat(AL, DI) ::
      //
      Move(0x1.toByte, AL) ::
      Move((320*66).toShort, CX) ::
      StoreString.Repeat(AL, DI) ::
      //
      Move(0x2.toByte, AL) ::
      Move((320*67).toShort, CX) ::
      StoreString.Repeat(AL, DI) ::

      { implicit val label: UniqueLabel = Label.unique; Jump(label) } ::
      Nil
    )

    val path = Paths.get(System.getProperty("java.io.tmpdir"))
    val outputPath = path.resolve("bootFlags-output")
    Files.createDirectories(outputPath)
    val outputFilePath = outputPath.resolve("test.com")
    val out = new FileOutputStream(outputFilePath.toFile)

    val executable = Raw(section, 0)
    executable.encodableSections.head.finalContent.foreach { x => Console.println(s"${x.encodeByte.hexString} $x") }
    out.write(executable.encodeByte.toArray)
    Console.println(s"output to file $outputFilePath")
    out.flush()

    //objdump -b binary -D /tmp/bootFlags-output/test.com -m i8086 -M intel
  }

}
