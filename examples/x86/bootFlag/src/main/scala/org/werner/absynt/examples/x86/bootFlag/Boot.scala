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

package org.werner.absynt.examples.x86.bootFlag

import org.werner.absynt.ListExtensions._
import org.werner.absynt.output.raw.Raw
import org.werner.absynt.resource.EncodableConversion._
import org.werner.absynt.resource.Resource
import org.werner.absynt.sections.Section
import org.werner.absynt.x86.ProcessorMode
import org.werner.absynt.x86.operands.ByteSize
import org.werner.absynt.{Label, UniqueLabel}

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}

object Boot extends App {
  createFile()

  case class Color(r: Byte, g: Byte, b: Byte)

  import ProcessorMode.Real._

  def setColor(col: Color): List[Resource] =
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
      Pop(FS) ::
        Pop.Unaligned(FS) ::
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
