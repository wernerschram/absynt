package assembler.x86.operations

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operations.OperandInfo.OperandOrder._

class ModRRM[Size <: ValueSize](val register: GeneralPurposeRegister with Size,
                                operandRM: ModRMEncodableOperand with Size,
                                override val code: Seq[Byte],
                                override val mnemonic: String,
                                override val operandRMOrder: OperandOrder)
                               (override implicit val processorMode: ProcessorMode)
  extends ModRM(operandRM, code, register.registerOrMemoryModeCode, mnemonic, operandRMOrder) with NoDisplacement with NoImmediate {

  def operandROrder: OperandOrder =
    if (operandRMOrder == destination) source else destination

  override protected def modRMInit(): Unit = {
    super.modRMInit()
    addOperand(OperandInfo.rmRegister(register, operandROrder))
  }
}

