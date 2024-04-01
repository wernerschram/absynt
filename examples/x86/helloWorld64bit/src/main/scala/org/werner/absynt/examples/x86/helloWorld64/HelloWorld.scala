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

package org.werner.absynt.examples.x86.helloWorld64

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}

import org.werner.absynt.ListExtensions._
import org.werner.absynt.output.Elf.{Architecture, Executable}
import org.werner.absynt.resource.EncodableConversion._
import org.werner.absynt.resource.{AbsoluteReference, RelativeReference}
import org.werner.absynt.sections.Section
import org.werner.absynt.x86.ProcessorMode
import org.werner.absynt.x86.operands.DoubleWordSize
import org.werner.absynt.{EncodedBytes, EncodedString, Label}
import scala.language.implicitConversions

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
        Pop(ES) ::
        Pop(FS) ::
        EncodedBytes(0x66.toByte) ::
        Pop(FS) ::
        EncodedBytes(Seq(0x66.toByte, 0x48.toByte)) ::
        Pop(FS) ::
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
