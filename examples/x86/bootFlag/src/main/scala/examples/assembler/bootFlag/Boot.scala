package examples.assembler.bootFlag

import java.io.FileOutputStream

import assembler.Encodable
import assembler.x86.instructions._
import assembler.x86.operands.Register._
import assembler.ListExtensions._
import assembler.sections.Section
import assembler.x86.ProcessorMode

object Boot extends App {
  createFile()

  case class Color(val r: Byte, val g: Byte, val b: Byte)

  def SetColor(col: Color)(implicit processorMode: ProcessorMode) =
        Move(col.r, AL) ::
        Output(AL, 0x3c9) ::
        Move(col.g, AL) ::
        Output(AL, 0x3c9) ::
        Move(col.b, AL) ::
        Output(AL, 0x3c9) :: Nil

  def createFile() = {

    implicit val processorMode = ProcessorMode.Long

    val col1 = Color(63, 0, 0)
    val col2 = Color(63, 63, 63)
    val col3 = Color(0, 0, 63)

    val page: Section = Section(
      Move(0x13.toShort, AX) ::
      Interrupt(0x10.toByte) ::
        //
        Xor(AL, AL) ::
        Move(0x3c8, DX) ::
        Output(AL, DX) ::
        //
        SetColor(col1) :::
        SetColor(col2) :::
        SetColor(col3) :::
        //
        Xor(DI, DI) ::
        Move(0xa000.toByte, AX) ::
        Move(AX, DS) ::
        //
        Move(0x0, AX) ::
        Move(320*66, CX) ::
        StoreString.Repeat(AX, DI) ::
        //
        Move(0x1, AX) ::
        Move(320*67, CX) ::
        StoreString.Repeat(AX, DI) ::
        //
        Move(0x2, AX) ::
        Move(320*66, CX) ::
        StoreString.Repeat(AX, DI) ::
      Nil
    )

    val out = new FileOutputStream("assembler-output/test.com")
    page.content.collect { case x: Encodable => x }.foreach { x => Console.println(s"${x.encodeByte()(page).hexString} ${x}") }
    out.write(page.encodeByte()(page).toArray)
    out.flush()
    //objdump -b binary -D test.com -m i8086 -M intel
  }

}
