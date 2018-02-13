package assembler.x86.operands

sealed class OperandSize

sealed class ValueSize(override val toString: String) extends OperandSize

sealed class FarPointerSize(override val toString: String) extends OperandSize

object ValueSize {
  case object Byte extends ValueSize("BYTE")

  case object Word extends ValueSize("WORD")

  case object DoubleWord extends ValueSize("DWORD")

  case object QuadWord extends ValueSize("QWORD")

}

object FarPointerSize {
  case object DoubleWord extends FarPointerSize("DWORD")

  case object FarWord extends FarPointerSize("FWORD")
}
