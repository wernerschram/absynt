package assembler.x86.operations

trait Repeated {
  self: X86Operation =>
  override val repeated: Boolean = true
}
