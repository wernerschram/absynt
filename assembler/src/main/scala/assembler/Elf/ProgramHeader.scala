package assembler.Elf

import assembler.sections.Section

class ProgramHeader(section: Section)(implicit elf: Elf[_]) {
  def `type`: ProgramType = ProgramType.Load
  def flags: Flags[ProgramFlag] = ProgramFlag.Execute | ProgramFlag.Read
  def alignBytes: Int = 0x40
  def physicalAddressBytes: List[Byte] = elf.`class`.numberBytes(elf.getBaseAddress(section))

  def segmentFileOffset: List[Byte] = elf.`class`.numberBytes(elf.fileOffset(section))
  def segmentMemoryOffset: List[Byte] = elf.`class`.numberBytes(elf.getBaseAddress(section))
  def segmentFileSize: List[Byte] = elf.`class`.numberBytes(section.size)

  def segmentMemorySize: List[Byte] = segmentFileSize

  implicit def endianness: Endianness = elf.endianness

  def header: List[Byte] = elf.`class` match {
    case ElfClass._32Bit =>
      elf.endianness.encode(`type`.id) :::
      segmentFileOffset :::
      segmentMemoryOffset :::
      physicalAddressBytes :::
      segmentFileSize :::
      segmentMemorySize :::
      elf.endianness.encode(flags.encode.toInt) :::
      elf.`class`.numberBytes(alignBytes)
    case ElfClass._64Bit =>
      elf.endianness.encode(`type`.id) :::
      elf.endianness.encode(flags.encode.toInt) :::
      segmentFileOffset :::
      segmentMemoryOffset :::
      physicalAddressBytes :::
      segmentFileSize :::
      segmentMemorySize :::
      elf.`class`.numberBytes(alignBytes)
  }
}

object ProgramHeader {
  def apply(section: Section)(implicit elf: Elf[_]): ProgramHeader = new ProgramHeader(section)
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
