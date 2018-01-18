package assembler.elf

import assembler._
import assembler.ListExtensions._
import assembler.output.Elf._
import assembler.sections.{Section, SectionType}
import org.scalatest.{Matchers, WordSpec}

class ElfSuite extends WordSpec with Matchers {

  def filler(count: Int): EncodedBytes =
    EncodedBytes(Seq.fill(count)(0x00.toByte))

  "an X86 (32-bit) Elf file with some sections" should {
    val entry: Label = Label.unique

    val sectionProperties = Seq(
      (SectionType.Text, "TextName", Seq[Byte](0), entry),
      (SectionType.Data, "DataName", Seq[Byte](0), Label.noLabel)
    )

    val sections = sectionProperties.map { case (sectionType, name, content, label) =>
      Section(sectionType, name, EncodedBytes(content).label(label) :: Nil)
    }

    val executable = Executable(Architecture.X86, sections.toList, entry, 0)

    "return a valid stringMap" in {
      executable.stringMap shouldBe Map("" -> 0, "TextName" -> 1, "DataName" -> 10, ".shstrtab" -> 19)
    }

    "provide the correct endianness" in {
      executable.endianness shouldBe Endianness.LittleEndian
    }

    "provide the correct elf file type" in {
      executable.elfType shouldBe ElfType.Executable
    }

    "have a program header for each section" in {
      executable.programHeaders.map(_.section) shouldBe sections
    }

    "have a null section header" in {
     executable.sectionHeaders.collect{case x: NullSectionHeader => x}.size shouldBe 1
    }

    "have a section header for each section" in {
      executable.sectionHeaders.collect{case x: SectionSectionHeader => x}.map(_.section) shouldBe sections
    }

    "have a stringtable section header" in {
      executable.sectionHeaders.collect{case x: StringSectionHeader => x}.size shouldBe 1
    }

    "return all resources in the correct order" in {

      val applicationHeaderSize: Short = 52
      val programHeaderSize: Short = 32
      val programHeaderCount: Short = sections.length.toShort
      val sectionHeaderSize: Short = 40
      val sectionHeaderCount: Short = (sections.length + 2).toShort

      val sectionAlignment = sectionProperties
        .foldLeft((applicationHeaderSize + programHeaderSize * programHeaderCount, Seq.empty[Int]))((total, current) => {
        val alignment = if (total._1 % 16 == 0) 0 else 16 - total._1 % 16
        (total._1 + alignment + current._3.length, total._2 :+ alignment)
      })._2

      val entryOffset = applicationHeaderSize + programHeaderSize * programHeaderCount + sectionAlignment.head
      val programHeaderOffset = applicationHeaderSize

      val section1FileOffset = 0
      val section1MemoryOffset = 0
      val section1Size = applicationHeaderSize + programHeaderCount * programHeaderSize + sectionAlignment.head + sectionProperties.head._3.length
      val section2FileOffset = section1Size + sectionAlignment(1)
      val section2MemoryOffset = section1Size + sectionAlignment(1) + 0x1000
      val section2Size = sectionProperties(1)._3.length
      val stringSectionFileOffset = section2FileOffset + section2Size
      val stringSectionMemoryOffset = 0
      val stringSectionSize = 1 + sectionProperties.map(_._2.length + 1).sum + 10

      val sectionHeaderOffset = stringSectionFileOffset + stringSectionSize

      val programHeader =
        (0x7F.toByte +: "ELF".toCharArray.map(_.toByte)) ++
        Seq[Byte](1, 1, 1, 0, 0).flatMap(_.encodeLittleEndian) ++
        Seq.fill[Byte](7)(0).flatMap(_.encodeLittleEndian) ++
        Seq[Short](2, 3).flatMap(_.encodeLittleEndian) ++
        Seq[Int](1, entryOffset, programHeaderOffset, sectionHeaderOffset, 0).flatMap(_.encodeLittleEndian) ++
        Seq[Short](
          applicationHeaderSize,
          programHeaderSize,
          programHeaderCount,
          sectionHeaderSize,
          sectionHeaderCount,
          (sections.size + 1).toShort).flatMap(_.encodeLittleEndian)

      val applicationHeaders = Seq(
        Seq(1, section1FileOffset, section1MemoryOffset, section1MemoryOffset, section1Size, section1Size, 5, 0x1000),
        Seq(1, section2FileOffset, section2MemoryOffset, section2MemoryOffset, section2Size, section2Size, 6, 0x1000)
      ).map(header => header.flatMap(_.encodeLittleEndian))

      val stringSection = ("" +: sectionProperties.map(_._2) :+ ".shstrtab").flatMap(name => s"${name}\u0000".toCharArray.map(_.toByte))

      val content =
        sectionAlignment.zip(sectionProperties.map(_._3)).flatMap{case (alignment, section) => Seq.fill(alignment)(0) ++ section} ++
          stringSection

      val sectionHeaders = Seq(
        Seq.fill(40)(0), // null
        Seq(1, 1, 6, section1MemoryOffset, section1FileOffset, section1Size, 0, 0, 16, 0).flatMap(_.encodeLittleEndian), // Section1
        Seq(10, 1, 3, section2MemoryOffset, section2FileOffset, section2Size, 0, 0, 16, 0).flatMap(_.encodeLittleEndian), // Section2
        Seq(19, 3, 0, stringSectionMemoryOffset, stringSectionFileOffset, stringSectionSize, 0, 0, 1, 1).flatMap(_.encodeLittleEndian) // strings
      )

      executable.encodeByte shouldBe
        programHeader ++
        applicationHeaders.flatten ++
        content ++
        sectionHeaders.flatten
    }
  }
}
