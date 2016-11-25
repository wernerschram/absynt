package assembler.x86.operations

import assembler.memory.MemoryPage
import assembler.x86.operands.ImmediateValue

trait Immediate extends X86Operation {

  self: X86Operation =>
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
