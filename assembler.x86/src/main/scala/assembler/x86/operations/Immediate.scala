package assembler.x86.operations

import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.ParameterPosition
import assembler.x86.operands.ImmediateValue
import assembler.x86.ProcessorMode
import assembler.memory.MemoryPage
import assembler.x86.operands.Operand
import assembler.x86.instructions.FixedSizeX86Operation
import assembler.x86.instructions.FixedSizeX86Operation2

trait Immediate extends FixedSizeX86Operation2 {

  self: FixedSizeX86Operation2 =>
  def immediate: ImmediateValue

  abstract override def operands = super.operands ::: immediate :: Nil

  abstract override def operandSize: Option[Int] = super.operandSize match {
    case size: Some[Int] => size
    case None => Some(immediate.operandByteSize)
  }

  abstract override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
    super.encodeByte() ::: immediate.value
  }

}
