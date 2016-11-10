package assembler.x86.opcodes

import assembler.x86.ParameterPosition
import assembler.x86.operands.EncodableRegister
import assembler.x86.operands.ImmediateValue
import assembler.x86.ProcessorMode

class RegisterEncoded[RegisterType <: EncodableRegister](code: List[Byte], override val includeRexW: Boolean = true)(implicit val mnemonic: String)
  extends OneOperand[RegisterType] {

  val parameterPosition = ParameterPosition.OpcodeReg

  override def getCode(operand: RegisterType): List[Byte] =
    code.take(code.length - 1) ::: (code.last | operand.registerOrMemoryModeCode).toByte :: Nil
}

class RegisterEncodedWithImmediate[RegisterType <: EncodableRegister](code: List[Byte], val rValue: Byte = 0,
  validateExtension: PartialFunction[(RegisterType, ImmediateValue, ProcessorMode), Boolean] = TwoOperand.valid, includeRexW: Boolean = true)(implicit val mnemonic: String)
    extends TwoOperand[RegisterType, ImmediateValue] {
  override val parameter1Position = ParameterPosition.OperandRM
  override val parameter2Position = ParameterPosition.NotEncoded

  override def validate(operand: RegisterType, immediate: ImmediateValue)(implicit processorMode: ProcessorMode): Boolean =
    super.validate(operand, immediate) && validateExtension(operand, immediate, processorMode)

  override def getCode(operand: RegisterType, immediate: ImmediateValue): List[Byte] =
    code.take(code.length - 1) ::: (code.last | operand.registerOrMemoryModeCode).toByte :: Nil ::: immediate.value

}
