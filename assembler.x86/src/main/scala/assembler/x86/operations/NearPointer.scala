package assembler.x86.operations

import assembler.memory.MemoryPage
import assembler.x86.ParameterPosition
import assembler.x86.operands.memoryaccess.{ NearPointer => NearPointerType }

trait NearPointer extends X86Operation {

  self: X86Operation =>
  def pointer: NearPointerType

  abstract override def operands = super.operands ::: pointer :: Nil

  abstract override def operandSize: Option[Int] = super.operandSize match {
    case size: Some[Int] => size
    case None => Some(pointer.operandByteSize)
  }

  override def validate = {
    super.validate
    assume(pointer.isValidForMode(processorMode))
  }

  abstract override def rexRequirements = pointer.getRexRequirements(ParameterPosition.NotEncoded) ::: super.rexRequirements

  abstract override def encodeByte()(implicit page: MemoryPage): List[Byte] =
    super.encodeByte() ::: pointer.displacement
}
