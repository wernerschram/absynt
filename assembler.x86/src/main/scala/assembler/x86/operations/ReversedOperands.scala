package assembler.x86.operations

trait ReversedOperands
    extends X86Operation {
  override def toString() = s"${mnemonic} ${operands.map { operand => operand.toString() }.mkString(", ")}"
}
