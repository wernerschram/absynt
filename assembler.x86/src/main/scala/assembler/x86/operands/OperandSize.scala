package assembler.x86.operands

sealed class OperandSize

trait ValueSize {
  def sizeName: String
  override def toString = s"$sizeName PTR ${super.toString}"
  def sizeEquals(that: ValueSize): Boolean
}

trait DisplacementSize extends ValueSize // 8, 16, 32

trait WideSize extends ValueSize //16, 32, 64

trait ExtendedSize extends WideSize //16, 32

trait LongSize extends WideSize //32, 64

trait ByteSize extends ValueSize with DisplacementSize {
  override val sizeName = "BYTE"
  def sizeEquals(that: ValueSize): Boolean = that.isInstanceOf[ByteSize]
}

trait WordSize extends ExtendedSize with DisplacementSize {
  override val sizeName = "WORD"
  def sizeEquals(that: ValueSize): Boolean = that.isInstanceOf[WordSize]
}

trait DoubleWordSize extends ExtendedSize with LongSize with DisplacementSize {
  override val sizeName = "DWORD"
  def sizeEquals(that: ValueSize): Boolean = that.isInstanceOf[DoubleWordSize]
}

trait QuadWordSize extends LongSize {
  override val sizeName = "QWORD"
  def sizeEquals(that: ValueSize): Boolean = that.isInstanceOf[QuadWordSize]
}

// TODO: Implement and test toString for FarPointerSize
sealed trait FarPointerSize

trait FarDoubleWordSize extends FarPointerSize

trait FarWordSize extends FarPointerSize

