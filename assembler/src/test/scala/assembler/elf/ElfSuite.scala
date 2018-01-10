package assembler.elf

import assembler._
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

    "return all resources in the correct order" in {
      executable.encodeByte shouldBe List(
        //program header
        127, 69, 76, 70,
        1,
        1,
        1,
        0,
        0,
        0, 0, 0, 0, 0, 0, 0,
        2, 0,
        3, 0,
        1, 0, 0, 0,
        -128, 0, 0, 0,
        52, 0, 0, 0,
        -82, 0, 0, 0,
        0, 0, 0, 0,
        52, 0,
        32, 0,
        2, 0,
        40, 0,
        4, 0,
        3, 0,

        // application header (section1)
        1, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        -127, 0, 0, 0,
        -127, 0, 0, 0,
        5, 0, 0, 0,
        0, 16, 0, 0,

        // application header (section2)
        1, 0, 0, 0,
        -112, 0, 0, 0,
        -112, 16, 0, 0,
        -112, 16, 0, 0,
        1, 0, 0, 0,
        1, 0, 0, 0,
        6, 0, 0, 0,
        0, 16, 0, 0,

        // allignment, section1, alignment, section2
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0,

        // string section
        // null string
        0,
        // "TextName"
        84, 101, 120, 116, 78, 97, 109, 101, 0,
        // "DataName"
        68, 97, 116, 97, 78, 97, 109, 101, 0,
        // ".shstrtab"
        46, 115, 104, 115, 116, 114, 116, 97, 98, 0,

        // section header (null section)
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,

        // section header (section1)
        1, 0, 0, 0,
        1, 0, 0, 0,
        6, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        -127, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        16, 0, 0, 0,
        0, 0, 0, 0,

        // section header (section2)
        10, 0, 0, 0,
        1, 0, 0, 0,
        3, 0, 0, 0,
        -112, 16, 0, 0,
        -112, 0, 0, 0,
        1, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        16, 0, 0, 0,
        0, 0, 0, 0,

        // section header (string section)
        19, 0, 0, 0,
        3, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        -111, 0, 0, 0,
        29, 0, 0, 0,
        0, 0, 0, 0,
        0, 0, 0, 0,
        1, 0, 0, 0,
        1, 0, 0, 0
      )
    }
  }
}
