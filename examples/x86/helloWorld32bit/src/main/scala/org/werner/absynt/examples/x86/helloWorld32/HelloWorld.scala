/*
 * Copyright 2019 Werner Schram
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

package org.werner.absynt.examples.x86.helloWorld32

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}

import org.werner.absynt.ListExtensions._
import org.werner.absynt.output.Elf.{Architecture, Executable}
import org.werner.absynt.resource.EncodableConversion._
import org.werner.absynt.resource.{AbsoluteReference, RelativeReference}
import org.werner.absynt.sections.Section
import org.werner.absynt.x86.ProcessorMode
import org.werner.absynt.x86.operands.ByteSize
import org.werner.absynt.{EncodedBytes, EncodedString, Label}

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
      Move(RegisterMemoryLocation[ByteSize](BP+SI), AL) ::
      Nil
    )

    val text2: Section = Section.text(
        Convert.Split(AX) ::
        Convert.Split(EAX) ::
        Convert.ScaleUp(AL) ::
        Convert.ScaleUp(AX) ::
        Push(FS) ::
          EncodedBytes(0x66.toByte) ::
          Push(FS) ::
          Pop(FS) ::
          EncodedBytes(0x66.toByte) ::
          Pop(FS) ::
          Pop(GS) ::
        EncodedBytes(0x66.toByte) ::
        Pop(GS) ::
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

    val exec = Executable(Architecture.X86, text :: text2 :: data :: Nil, entry, 0x8048000)
    (text2.content zip text2.content.encodables(exec.encodablesForDependencies(text.content.dependentResources))).foreach {
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
