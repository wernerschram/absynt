package assembler.x86.operands

sealed abstract class ReturnMode extends Operand with FixedSizeOperand

object ReturnMode {
  object Protected extends ReturnMode {
    override def toString: String = ""

    override val operandByteSize: OperandSize = ValueSize.DoubleWord
  }
  object Long extends ReturnMode {
    override def toString: String = "q"

    override val operandByteSize: OperandSize = ValueSize.QuadWord
  }
}
