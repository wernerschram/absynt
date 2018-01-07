package assembler.x86.operations

trait Repeated extends X86Operation {
  self: X86Operation =>

  abstract override def encodeByte: Seq[Byte] = {
    0xF3.toByte +: super.encodeByte
  }

  abstract override def mnemonic = s"rep ${super.mnemonic}"
}
