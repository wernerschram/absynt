package assembler.elf

import assembler.output.Elf
import assembler.{sections, _}
import assembler.output.Elf.SectionType.Null
import assembler.output.Elf._
import assembler.sections.{Section, SectionType}
import org.scalatest.{Matchers, WordSpec}

class ElfSuite extends WordSpec with Matchers {

  def filler(count: Int, label: Label = Label.noLabel): EncodedBytes =
    EncodedBytes(Seq.fill(count)(0x00.toByte))(label)

  "an X86 (32-bit) Elf file with some sections" should {
    val entry: Label = Label.unique
    val section1 = Section(SectionType.Text, "TextName", filler(1, entry) :: Nil)
    val section2 = Section(SectionType.Data, "DataName", filler(1) :: Nil)

    val executable = Executable(Architecture.X86, List(section1, section2), entry, 0)

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
      executable.programHeaders.map(_.section) shouldBe Seq(section1, section2)
    }

    "have a null section header" in {
      executable.sectionHeaders.collect{case x: NullSectionHeader => x}.size shouldBe 1
    }

    "have a section header for each section" in {
      executable.sectionHeaders.collect{case x: SectionSectionHeader => x}.map(_.section) shouldBe Seq(section1, section2)
    }

    "have a stringtable section header" in {
      executable.sectionHeaders.collect{case x: StringSectionHeader => x}.size shouldBe 1
    }

  }
}
