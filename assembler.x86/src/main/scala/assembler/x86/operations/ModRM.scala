package assembler.x86.operations

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operations.OperandInfo.OperandOrder.OperandOrder

abstract class ModRM[Size<:ValueSize](val operandRM: ModRMEncodableOperand with Size,
                     override val code: Seq[Byte],
                     val rValue: Byte,
                     override val mnemonic: String,
                     val operandRMOrder: OperandOrder,
                     includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
  extends X86Operation(code) with ModRMBytes {

  self: X86Operation with DisplacementBytes with ImmediateBytes =>

  override def modRMBytes: Seq[Byte] = operandRM.getExtendedBytes(rValue)

  override protected def modRMInit(): Unit =
    addOperand(OperandInfo.rmRegisterOrMemory(operandRM, operandRMOrder, includeRexW))
}

