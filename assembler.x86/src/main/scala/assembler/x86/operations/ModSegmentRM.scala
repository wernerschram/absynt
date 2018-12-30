package assembler.x86.operations

import assembler.x86.ProcessorMode
import assembler.x86.operands.{ModRMEncodableOperand, SegmentRegister}
import assembler.x86.operations.OperandInfo.OperandOrder._

abstract class ModSegmentRM(val register: SegmentRegister,
                            operandRM: ModRMEncodableOperand,
                            override val code: Seq[Byte],
                            override val mnemonic: String,
                            override val operandRMOrder: OperandOrder)(override implicit val processorMode: ProcessorMode)
  extends ModRM(operandRM, code, register.registerCode, mnemonic, operandRMOrder) {
  self: X86Operation with DisplacementBytes with ImmediateBytes =>

  def operandSegmentOrder: OperandOrder =
    if (operandRMOrder == destination) source else destination

  override def operands: Set[OperandInfo] = super.operands + OperandInfo.rmSegment(register, operandSegmentOrder)
}
