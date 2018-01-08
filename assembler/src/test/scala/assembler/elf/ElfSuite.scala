package assembler.elf

import assembler._
import assembler.output.Elf.{Architecture, Executable}
import assembler.sections.{Section, SectionType}
import org.scalatest.{Matchers, WordSpec}

class ElfSuite extends WordSpec with Matchers {

  "an Elf file" when {

    def filler(count: Int): EncodedBytes =
      EncodedBytes(Seq.fill(count)(0x00.toByte))

    "provided with some sections" should {

      "return a valid stringMap" in {
        val entry: Label = Label.unique
        val section1 = Section(SectionType.Text, "TextName", filler(1) :: Nil)
        val section2 = Section(SectionType.Data, "DataName", filler(1) :: Nil)

        val executable = Executable(Architecture.X86, List(section1, section2), entry, 0)
        executable.stringMap shouldBe Map("" -> 0, "TextName" -> 1, "DataName" -> 10, ".shstrtab" -> 19)
      }
    }
  }
}
