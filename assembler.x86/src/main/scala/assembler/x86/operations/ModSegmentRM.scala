package assembler.x86.operations

import assembler.x86.ProcessorMode
import assembler.x86.operands.{ModRMEncodableOperand, SegmentRegister, WideSize, WordSize}
import assembler.x86.operations.OperandInfo.OperandOrder._

abstract class ModSegmentRM[Size<:WideSize](val register: SegmentRegister,
                            operandRM: ModRMEncodableOperand with Size,
                            override val code: Seq[Byte],
                            override val mnemonic: String,
                            override val operandRMOrder: OperandOrder)(override implicit val processorMode: ProcessorMode)
  extends ModRM(operandRM, code, register.registerCode, mnemonic, operandRMOrder) {
  self: X86Operation with DisplacementBytes with ImmediateBytes =>

  def operandSegmentOrder: OperandOrder =
    if (operandRMOrder == destination) source else destination

  override protected def modRMInit(): Unit = {
    super.modRMInit()
    addOperand(OperandInfo.rmSegment(register, operandSegmentOrder))
  }
}
