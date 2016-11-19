package assembler.x86.operations

import assembler.memory.MemoryPage
import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.SegmentRegister
import assembler.x86.operands.FixedSizeEncodableOperand

class ModSegmentRMStaticOperation(
  override val operand1: SegmentRegister,
  override val operand2: ModRMEncodableOperand,
  override val code: List[Byte],
  override val mnemonic: String,
  override val includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
    extends OneOperandOperation[SegmentRegister] with SecondOperand[SegmentRegister, ModRMEncodableOperand] {

  override val parameter1Position = ParameterPosition.OperandR
  override val parameter2Position = ParameterPosition.OperandRM

//  override def validate =
//    operand2 match {
//      case fixed: FixedSizeEncodableOperand => super.validate; assume(fixed.operandByteSize != 1)
//      case _ =>
//  }

  override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
    super.encodeByte() ::: operand2.getExtendedBytes(operand1)
  }
}
