package examples.assembler.x86

import java.io.FileOutputStream

import assembler.Encodable
import assembler.sections.Section
import assembler.ListExtensions._

import assembler.x86.ProcessorMode
import assembler.x86.instructions._
import assembler.x86.operands.Register._
import assembler.x86.operands.ImmediateValue
import assembler.x86.operands.ImmediateValue._
import assembler.x86.operands.memoryaccess.RegisterMemoryLocation
import assembler.x86.operands.memoryaccess.MemoryAddress
import assembler.x86.operands.memoryaccess.SIBMemoryLocation

object Boot extends App {
  createFile()

  def createFile() = {

    implicit val processorMode = ProcessorMode.Long

    val page: Section = new Section(
        Move(EBP, RegisterMemoryLocation(RDX)) ::
        Move(EBP, RegisterMemoryLocation(R9)) ::
        Move(EBP, RegisterMemoryLocation(EDX)) ::
        Move(EBP, SIBMemoryLocation(R9, R8, 0.encodeLittleEndian, 2)) ::


      // Setup Stack
//      Move(0x13.toShort, AX) ::
//      Interrupt(0x10.toByte) ::
      Nil
    )

    val out = new FileOutputStream("c:\\temp\\test.com")
    page.content.collect { case x: Encodable => x }.foreach { x => Console.println(s"${x.encodeByte()(page).hexString()} ${x}") }
    out.write(page.encodeByte().toArray)
    out.flush()
    //objdump -b binary -D test.com -m i8086 -M intel
  }

}
