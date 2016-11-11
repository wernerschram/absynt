package assembler.x86.instructions

import assembler.Encodable
import assembler.Label
import assembler.LabeledEncodable

trait X86Operation extends Encodable {
  def withLabel(label: Label): LabeledEncodable
}