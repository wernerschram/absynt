package assembler.x86.instructions

import assembler.Encodable
import assembler.Label
import assembler.LabeledEncodable

abstract class X86Instruction() extends Encodable() {
  def withLabel(label: Label): LabeledEncodable
}