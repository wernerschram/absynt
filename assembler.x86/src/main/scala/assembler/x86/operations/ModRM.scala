package assembler.x86.operations

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operations.OperandInfo.OperandOrder.{OperandOrder, _}

sealed trait ModRMBytes {
  self: X86Operation =>
  def modRMBytes: Seq[Byte]
  private[operations] def modRMInit(): Unit
}

trait NoModRM extends ModRMBytes {
  self: X86Operation =>
  override def modRMBytes: Seq[Byte] = Nil
  override final def modRMInit(): Unit = Unit
}

class ModRM[Size<:ValueSize](val operandRM: ModRMEncodableOperand with Size,
                     override val code: Seq[Byte],
                     val rValue: Byte,
                     override val mnemonic: String,
                     val operandRMOrder: OperandOrder,
                     includeRexW: Boolean = true)(implicit override val processorMode: ProcessorMode, operandSizePrefixRequirement: OperandSizePrefixRequirement)
  extends X86Operation(code) with ModRMBytes {

  self: X86Operation with DisplacementBytes with ImmediateBytes =>

  override def modRMBytes: Seq[Byte] = operandRM.getExtendedBytes(rValue)

  override def modRMInit(): Unit =
    addOperand(OperandInfo.rmRegisterOrMemory(operandRM, operandRMOrder, includeRexW))
}

class ModRRM[Size <: ValueSize](val register: GeneralPurposeRegister with Size,
                                operandRM: ModRMEncodableOperand with Size,
                                override val code: Seq[Byte],
                                override val mnemonic: String,
                                override val operandRMOrder: OperandOrder)
                               (implicit override val processorMode: ProcessorMode, operandSizePrefixRequirement: OperandSizePrefixRequirement)
  extends ModRM(operandRM, code, register.registerOrMemoryModeCode, mnemonic, operandRMOrder) with NoDisplacement with NoImmediate {

  def operandROrder: OperandOrder =
    if (operandRMOrder == destination) source else destination

  override final def modRMInit(): Unit = {
    super.modRMInit()
    addOperand(OperandInfo.rmRegister(register, operandROrder))
  }
}

class ModSegmentRM[Size<:WordDoubleQuadSize](val register: SegmentRegister,
                                             operandRM: ModRMEncodableOperand with Size,
                                             override val code: Seq[Byte],
                                             override val mnemonic: String,
                                             override val operandRMOrder: OperandOrder)
                                            (implicit override val processorMode: ProcessorMode, operandSizePrefixRequirement: OperandSizePrefixRequirement)
  extends ModRM(operandRM, code, register.registerCode, mnemonic, operandRMOrder) {
  self: X86Operation with DisplacementBytes with ImmediateBytes =>

  def operandSegmentOrder: OperandOrder =
    if (operandRMOrder == destination) source else destination

  override final def modRMInit(): Unit = {
    super.modRMInit()
    addOperand(OperandInfo.rmSegment(register, operandSegmentOrder))
  }
}
