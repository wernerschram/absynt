package assembler.x86.operations

import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.ParameterPosition
import assembler.x86.operands.ImmediateValue
import assembler.x86.ProcessorMode
import assembler.memory.MemoryPage
import assembler.x86.operands.Operand
import assembler.x86.instructions.FixedSizeX86Operation
import assembler.x86.instructions.FixedSizeX86Operation2
import assembler.x86.operands.memoryaccess.{NearPointer => NearPointerType}

trait NearPointer extends FixedSizeX86Operation2 {

  self: FixedSizeX86Operation2 =>
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


//  def withNearPointer(validateExtension: PartialFunction[(NearPointer, ProcessorMode), Boolean] = OneOperand.valid): OneOperand[NearPointer] =
//    new OneOperand[NearPointer] {
//
//        super.validate(pointer) && validateExtension(pointer, processorMode)
//
//      override def getCode(pointer: NearPointer): List[Byte] =
//        NoOperand.this.getCode ::: pointer.displacement
//    }
