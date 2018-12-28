package assembler.x86.operations

import assembler.x86.ProcessorMode
import assembler.x86.operands.{ModRMEncodableOperand, SegmentRegister}
import assembler.x86.operations.OperandInfo.OperandOrder._

class ModSegmentRM(val register: SegmentRegister,
                            operandRM: ModRMEncodableOperand,
                            override val code: Seq[Byte],
                            override val mnemonic: String,
                            override val operandRMOrder: OperandOrder)(override implicit val processorMode: ProcessorMode)
  extends ModRM(operandRM, code, register.registerCode, mnemonic, operandRMOrder) {

  def operandSegmentOrder: OperandOrder =
    if (operandRMOrder == destination) source else destination

  override def operands: Set[OperandInfo] = super.operands + OperandInfo.rmSegment(register, operandSegmentOrder)
}
