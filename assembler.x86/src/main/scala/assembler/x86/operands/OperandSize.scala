package assembler.x86.operands

sealed trait OperandSize {
  def sizeEquals(that: OperandSize): Boolean
}

sealed trait ValueSize extends OperandSize {
  def sizeName: String
}

sealed trait ByteWordDoubleSize extends ValueSize // 8, 16, 32

sealed trait WordDoubleQuadSize extends ValueSize //16, 32, 64

sealed trait ByteWordSize extends ByteWordDoubleSize // 8, 16

sealed trait WordDoubleSize extends ByteWordDoubleSize with WordDoubleQuadSize //16, 32

sealed trait WordQuadSize extends WordDoubleQuadSize //16, 64

sealed trait DoubleQuadSize extends WordDoubleQuadSize //32, 64

trait ByteSize extends ValueSize with ByteWordSize with ByteWordDoubleSize {
  override val sizeName = "BYTE"
  def sizeEquals(that: OperandSize): Boolean = that.isInstanceOf[ByteSize]
}

trait WordSize extends ByteWordSize with WordDoubleSize {
  override val sizeName = "WORD"
  def sizeEquals(that: OperandSize): Boolean = that.isInstanceOf[WordSize]
}

trait DoubleWordSize extends WordDoubleSize with DoubleQuadSize {
  override val sizeName = "DWORD"
  def sizeEquals(that: OperandSize): Boolean = that.isInstanceOf[DoubleWordSize]
}

trait QuadWordSize extends DoubleQuadSize {
  override val sizeName = "QWORD"
  def sizeEquals(that: OperandSize): Boolean = that.isInstanceOf[QuadWordSize]
}

sealed trait FarPointerSize[OffsetSize<:WordDoubleSize] extends OperandSize {
  val offset: OffsetSize
  override def sizeEquals(that: OperandSize): Boolean = that.isInstanceOf[FarPointerSize[OffsetSize]]
}

trait FarWordSize extends FarPointerSize[WordSize]

trait FarDoubleWordSize extends FarPointerSize[DoubleWordSize]

