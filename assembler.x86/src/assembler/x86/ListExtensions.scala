package assembler.x86

import assembler.x86.operands.ImmediateValue

object ListExtensions {
  implicit class ListToImmediate(value: List[Byte]) {
    def toImmediate : ImmediateValue = new ImmediateValue(value)
  }
}
