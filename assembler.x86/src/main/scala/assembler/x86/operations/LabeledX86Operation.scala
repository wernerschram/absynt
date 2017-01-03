package assembler.x86.operations

import assembler.{Label, LabeledEncodable}

class LabeledX86Operation(override val value: X86Operation, override val label: Label) extends X86Operation with LabeledEncodable {
  val mnemonic: String = value.mnemonic

  def code: List[Byte] = value.code

  def operands: List[assembler.x86.operands.Operand] = value.operands

  implicit val processorMode: assembler.x86.ProcessorMode = value.processorMode
}
