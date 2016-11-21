package assembler.x86.operations

import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.ParameterPosition
import assembler.x86.operands.ImmediateValue
import assembler.x86.ProcessorMode
import assembler.memory.MemoryPage
import assembler.x86.operands.Operand
import assembler.x86.instructions.FixedSizeX86Operation
import assembler.x86.instructions.FixedSizeX86Operation2
import assembler.x86.operands.memoryaccess.{FarPointer => FarPointerType}

trait FarPointer extends FixedSizeX86Operation2 {

  self: FixedSizeX86Operation2 =>
  def pointer: FarPointerType

  abstract override def operands = super.operands ::: pointer :: Nil

  abstract override def operandSize: Option[Int] = super.operandSize match {
    case size: Some[Int] => size
    case None => Some(pointer.operandByteSize)
  }

  abstract override def rexRequirements = pointer.getRexRequirements(ParameterPosition.NotEncoded) ::: super.rexRequirements

  abstract override def encodeByte()(implicit page: MemoryPage): List[Byte] =
    super.encodeByte() ::: pointer.offset ::: pointer.segment
}
