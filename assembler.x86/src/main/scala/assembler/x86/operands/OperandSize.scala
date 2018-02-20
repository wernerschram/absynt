package assembler.x86.operands

sealed class OperandSize

//TODO Remove original ValueSize class and FixedSize traits
trait ValueSize2 extends FixedSizeOperand {
  def sizeName: String
  override def toString = s"$sizeName PTR ${super.toString}"
}

trait ByteSize extends ValueSize2 {
  override val sizeName = "BYTE"
  override val operandByteSize: OperandSize = ValueSize.Byte
}

trait WideSize extends ValueSize2 //16, 32, 64

trait ExtendedSize extends WideSize //16, 32

trait LongSize extends WideSize //32, 64

trait WordSize extends ExtendedSize {
  override val sizeName = "WORD"
  override val operandByteSize: OperandSize = ValueSize.Word
}

trait DoubleWordSize extends ExtendedSize with LongSize {
  override val sizeName = "DWORD"
  override val operandByteSize: OperandSize = ValueSize.DoubleWord
}

trait QuadWordSize extends LongSize {
  override val sizeName = "QWORD"
  override val operandByteSize: OperandSize = ValueSize.QuadWord
}

trait FarPointerSize2 {
  val farPointerSize: FarPointerSize
}

trait FarDoubleWordSize extends FarPointerSize2 {
  override val farPointerSize: FarPointerSize = FarPointerSize.DoubleWord
}

trait FarWordSize extends FarPointerSize2 {
  override val farPointerSize: FarPointerSize = FarPointerSize.FarWord
}

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
