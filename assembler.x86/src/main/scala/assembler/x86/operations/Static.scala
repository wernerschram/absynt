package assembler.x86.operations

import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Operation2

class Static(override val code: List[Byte], implicit val mnemonic: String)(override implicit val processorMode: ProcessorMode) extends FixedSizeX86Operation2 {
  def operands: List[assembler.x86.operands.Operand] = Nil
}