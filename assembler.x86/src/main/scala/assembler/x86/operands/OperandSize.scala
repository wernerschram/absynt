package assembler.x86.operands

sealed class OperandSize

sealed class ValueSize(override val toString: String) extends OperandSize

sealed class FarPointerSize(override val toString: String) extends OperandSize

object ValueSize {

  def sizeOfValue(size: Int): ValueSize = size match {
    case 1 => Byte
    case 2 => Word
    case 4 => DoubleWord
    case 8 => QuadWord
    case _ => throw new AssertionError
  }

  case object Byte extends ValueSize("BYTE")

  case object Word extends ValueSize("WORD")

  case object DoubleWord extends ValueSize("DWORD")

  case object QuadWord extends ValueSize("QWORD")

}

object FarPointerSize {

  def sizeOfFarPointer(segmentSize: Int, offsetSize: Int): FarPointerSize = (segmentSize, offsetSize) match {
    case (2, 2) => DoubleWord
    case (2, 4) => FarWord
    case _ => throw new AssertionError
  }

  case object DoubleWord extends FarPointerSize("DWORD")

  case object FarWord extends FarPointerSize("FWORD")

}
