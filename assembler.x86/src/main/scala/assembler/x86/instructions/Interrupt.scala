package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands.{ByteSize, ImmediateValue}
import assembler.x86.operations._
import assembler.x86.operations.OperandInfo.OperandOrder._

object Interrupt {

  private def Static(mnemonic: String)(implicit processorMode: ProcessorMode) = new Static(0xCC.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate  {
    override protected def implicitInit(): Unit =
      addOperand(OperandInfo.implicitOperand(ImmediateValue.forByte(3.toByte), destination))
  }

  private def Imm8(immediateValue: ImmediateValue with ByteSize, mnemonic: String)(implicit processorMode: ProcessorMode) =
    new Static(0xCD.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
      override def immediate: ImmediateValue with ByteSize = immediateValue

      override def immediateOrder: OperandOrder = destination
    }

  trait Operations {
    object Interrupt {
      val mnemonic: String = "int"

      def apply(immediate: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode): Static = immediate.value.head match {
        case 3 => Static(mnemonic)
        case _ => Imm8(immediate, mnemonic)
      }
    }
  }
}