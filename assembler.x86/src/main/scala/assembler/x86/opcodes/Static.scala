package assembler.x86.opcodes

class Static(code: List[Byte])(implicit val mnemonic: String) extends NoOperand {
  def getCode(): List[Byte] = code
}