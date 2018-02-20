package assembler.x86.operands

sealed class OperandSize

//TODO Remove original ValueSize class and FixedSize traits
trait ValueSize2 {
  def sizeName: String
  override def toString = s"$sizeName PTR ${super.toString}"
  def sizeEquals(that: ValueSize2): Boolean
}

trait WideSize extends ValueSize2 //16, 32, 64

trait ExtendedSize extends WideSize //16, 32

trait LongSize extends WideSize //32, 64

trait ByteSize extends ValueSize2 {
  override val sizeName = "BYTE"
  def sizeEquals(that: ValueSize2): Boolean = that.isInstanceOf[ByteSize]
}

trait WordSize extends ExtendedSize {
  override val sizeName = "WORD"
  def sizeEquals(that: ValueSize2): Boolean = that.isInstanceOf[WordSize]
}

trait DoubleWordSize extends ExtendedSize with LongSize {
  override val sizeName = "DWORD"
  def sizeEquals(that: ValueSize2): Boolean = that.isInstanceOf[DoubleWordSize]
}

trait QuadWordSize extends LongSize {
  override val sizeName = "QWORD"
  def sizeEquals(that: ValueSize2): Boolean = that.isInstanceOf[QuadWordSize]
}

// TODO: Implement and test toString for FarPointerSize
sealed trait FarPointerSize

trait FarDoubleWordSize extends FarPointerSize

trait FarWordSize extends FarPointerSize

