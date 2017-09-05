package assembler

import assembler.sections.Section

package object Elf {
  trait HasName[A <: Section] {
    def name(x: A): String
  }

}
