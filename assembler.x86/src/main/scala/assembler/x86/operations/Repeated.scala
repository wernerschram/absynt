package assembler.x86.operations

import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.ParameterPosition
import assembler.x86.operands.ImmediateValue
import assembler.x86.ProcessorMode
import assembler.memory.MemoryPage
import assembler.x86.operands.Operand
import assembler.x86.instructions.FixedSizeX86Operation
import assembler.x86.instructions.FixedSizeX86Operation2
import assembler.x86.opcodes.Opcode

trait Repeated extends FixedSizeX86Operation2 {

  self: FixedSizeX86Operation2 =>

  abstract override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
    Opcode.RepeatPrefix :: super.encodeByte()
  }

}
