package assembler.x86.operations

trait Repeated extends X86Operation {

  // TODO: remove extends X86Operation so that self type can be restricted
  self: X86Operation with ModRMBytes with DisplacementBytes with ImmediateBytes =>

  abstract override def encodeByte: Seq[Byte] = {
    0xF3.toByte +: super.encodeByte
  }

  abstract override def mnemonic = s"rep ${super.mnemonic}"
}
