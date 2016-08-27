package assembler.x86.opcodes

class Static(code: List[Byte])(implicit mnemonic: String) extends NoOperand(mnemonic) {

  def getCode(): List[Byte] = code
}