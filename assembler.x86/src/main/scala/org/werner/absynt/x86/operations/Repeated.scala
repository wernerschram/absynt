package org.werner.absynt.x86.operations

trait Repeated {
  self: X86Operation =>
  override val repeated: Boolean = true
}
