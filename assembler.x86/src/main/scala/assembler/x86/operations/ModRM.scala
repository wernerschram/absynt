package assembler.x86.operations

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operands.memoryaccess.{MemoryLocation => MemoryLocationType}
import assembler.x86.operations.OperandInfo.OperandOrder.OperandOrder

class ModRM(val operandRM: ModRMEncodableOperand,
                     override val code: Seq[Byte],
                     val rValue: Byte,
                     override val mnemonic: String,
                     val operandRMOrder: OperandOrder,
                     includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
  extends X86Operation {

  override def operands: Set[OperandInfo] = Set(OperandInfo.rmRegisterOrMemory(operandRM, operandRMOrder, includeRexW))

  override def encodeByte: Seq[Byte] =
    super.encodeByte ++ operandRM.getExtendedBytes(rValue)
}

