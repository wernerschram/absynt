package assembler.x86.opcodes

import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.operands.FixedSizeEncodableOperand
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.SegmentRegister

class ModSegmentRMStatic(code: List[Byte], includeRexW: Boolean = true)(implicit val mnemonic: String)
  extends TwoOperand[SegmentRegister, ModRMEncodableOperand] {

  val parameter1Position = ParameterPosition.OperandR
  val parameter2Position = ParameterPosition.OperandRM

  override def validate(operand1: SegmentRegister, operand2: ModRMEncodableOperand)(implicit processorMode: ProcessorMode): Boolean = operand2 match {
    case fixed: FixedSizeEncodableOperand => super.validate(operand1, operand2) && (fixed.operandByteSize != 1)
    case _ => true
  }

  def getCode(segment: SegmentRegister, operandRM: ModRMEncodableOperand): List[Byte] =
    code ::: operandRM.getExtendedBytes(segment)
}

