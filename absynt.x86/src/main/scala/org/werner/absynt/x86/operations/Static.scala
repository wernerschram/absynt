package org.werner.absynt.x86.operations

abstract class Static(override val code: Seq[Byte], opcode: String)
  extends X86Operation(code) with NoModRM {

  self: X86Operation with DisplacementBytes with ImmediateBytes =>
  def mnemonic: String = opcode
}