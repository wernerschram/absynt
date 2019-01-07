package assembler.x86.operands

sealed trait OperandSize {
  def sizeEquals(that: OperandSize): Boolean
}

trait ValueSize extends OperandSize {
  def sizeName: String
  override def toString = s"$sizeName PTR ${super.toString}"
}

trait DisplacementSize extends ValueSize // 8, 16, 32

trait WideSize extends ValueSize //16, 32, 64

trait ExtendedSize extends WideSize with DisplacementSize //16, 32

trait LongSize extends WideSize //32, 64

trait ByteSize extends ValueSize with DisplacementSize {
  override val sizeName = "BYTE"
  def sizeEquals(that: OperandSize): Boolean = that.isInstanceOf[ByteSize]
}

trait WordSize extends ExtendedSize {
  override val sizeName = "WORD"
  def sizeEquals(that: OperandSize): Boolean = that.isInstanceOf[WordSize]
}

trait DoubleWordSize extends ExtendedSize with LongSize {
  override val sizeName = "DWORD"
  def sizeEquals(that: OperandSize): Boolean = that.isInstanceOf[DoubleWordSize]
}

trait QuadWordSize extends LongSize {
  override val sizeName = "QWORD"
  def sizeEquals(that: OperandSize): Boolean = that.isInstanceOf[QuadWordSize]
}

sealed trait FarPointerSize[OffsetSize<:ExtendedSize] extends OperandSize {
  val offset: OffsetSize
  override def sizeEquals(that: OperandSize): Boolean = that match {
    case f: FarPointerSize[_] => offset.sizeEquals(f.offset)
    case _ => false
  }
}

trait FarWordSize extends FarPointerSize[WordSize]

trait FarDoubleWordSize extends FarPointerSize[DoubleWordSize]

