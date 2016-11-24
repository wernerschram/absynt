package assembler.x86.operations

import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.ParameterPosition
import assembler.x86.operands.ImmediateValue
import assembler.x86.ProcessorMode
import assembler.memory.MemoryPage
import assembler.x86.operands.Operand
import assembler.x86.instructions.FixedSizeX86Operation
import assembler.x86.instructions.FixedSizeX86Operation2
import assembler.x86.operands.memoryaccess.{MemoryLocation => MemoryLocationType}
import assembler.x86.operands.SegmentRegister

trait MemoryLocation extends FixedSizeX86Operation2 {

  self: FixedSizeX86Operation2 =>
  def location: MemoryLocationType

  abstract override def operands = super.operands ::: location :: Nil

  abstract override def addressSize: Option[Int] = super.operandSize match {
    case size: Some[Int] => size
    case None => Some(location.addressSize)
  }

  override def segmentOverride = super.segmentOverride match {
    case register: Some[SegmentRegister] => register
    case None => location.getSegmentOverride
  }


  abstract override def rexRequirements = location.getRexRequirements(ParameterPosition.NotEncoded) ::: super.rexRequirements

  abstract override def encodeByte()(implicit page: MemoryPage): List[Byte] =
    super.encodeByte() ::: location.displacement
}
