package assembler.output.Elf

import assembler.EncodedBytes
import assembler.resource.Resource
import assembler.sections.Section

class ProgramHeader(section: Section, val flags: Flags[ProgramFlag], elf: Elf) {
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
    section.sectionType match {
      case assembler.sections.SectionType.Text =>
        new ProgramHeader(section, ProgramFlag.Execute | ProgramFlag.Read, elf)
      case assembler.sections.SectionType.Data =>
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
