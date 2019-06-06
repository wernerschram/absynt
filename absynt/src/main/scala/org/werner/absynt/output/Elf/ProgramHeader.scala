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

package org.werner.absynt.output.Elf

import org.werner.absynt.EncodedBytes
import org.werner.absynt.resource.Resource
import org.werner.absynt.sections.{DataSection, Section, TextSection}

class ProgramHeader(val section: Section, val flags: Flags[ProgramFlag], elf: Elf) {
  def `type`: ProgramType = ProgramType.Load

  implicit def endianness: Endianness = elf.endianness

  def resources: Seq[Resource] = elf.architecture.processorClass match {
    case ProcessorClass._32Bit =>
      EncodedBytes(elf.endianness.encode(`type`.id)) ::
      ElfSectionFileReference(section, elf) ::
      ElfSectionReference(section, elf) ::
      ElfSectionReference(section, elf) ::
      ElfSectionSize(section, elf) ::
      ElfSectionSize(section, elf) ::
      EncodedBytes(elf.endianness.encode(flags.encode.toInt)) ::
      EncodedBytes(elf.architecture.processorClass.numberBytes(elf.fileAlignment)) ::
      Nil
    case ProcessorClass._64Bit =>
      EncodedBytes(elf.endianness.encode(`type`.id)) ::
      EncodedBytes(elf.endianness.encode(flags.encode.toInt)) ::
      ElfSectionFileReference(section, elf) ::
      ElfSectionReference(section, elf) ::
      ElfSectionReference(section, elf) ::
      ElfSectionSize(section, elf) ::
      ElfSectionSize(section, elf) ::
      EncodedBytes(elf.architecture.processorClass.numberBytes(elf.fileAlignment)) ::
      Nil
  }
}

object ProgramHeader {
  def apply(section: Section, elf: Elf): ProgramHeader =
    section match {
      case _: TextSection =>
        new ProgramHeader(section, ProgramFlag.Execute | ProgramFlag.Read, elf)
      case _: DataSection =>
        new ProgramHeader(section, ProgramFlag.Read | ProgramFlag.Write, elf)
    }
}

abstract case class ProgramType private(id: Int)

object ProgramType {
  object Null extends ProgramType(0)
  object Load extends ProgramType(1)
  object Dynamic extends ProgramType(2)
  object Interpret extends ProgramType(3)

  //...
}

abstract class Flags[+FlagsType] {
  def encode: Long

  def |[T >: FlagsType](other: Flags[T]): Flags[FlagsType] =
    new Flags[FlagsType] {
      override val encode: Long = Flags.this.encode | other.encode
    }
}

object Flags {
  case object None extends Flags[Nothing] {
    override val encode: Long = 0
  }
}

case class ProgramFlag private(flag: Long) extends Flags[ProgramFlag] {
  override def encode: Long = flag
}

object ProgramFlag {
  object Execute extends ProgramFlag(0x01)
  object Write extends ProgramFlag(0x02)
  object Read extends ProgramFlag(0x04)
  object MaskOS extends ProgramFlag(0x0ff00000)
  object MaskProcessor extends ProgramFlag(0xf0000000)
}
