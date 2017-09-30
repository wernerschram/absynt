package assembler.x86.operands.memoryaccess

import assembler.Address
import assembler.ListExtensions._
import assembler.x86.operands.{FixedSizeOperand, Operand, OperandSize, ValueSize}

sealed abstract class NearPointer[OffsetType:Numeric](val offset: OffsetType)
  extends Address[OffsetType] with Operand with FixedSizeOperand {
  val operandByteSize: OperandSize

  def encodeOffset: List[Byte]

  override def toString: String = s"0x${encodeOffset.bigEndianHexString}"

  def encodeBytes: List[Byte] = encodeOffset

  override def add(that: OffsetType): NearPointer[OffsetType] =
    new NearPointer[OffsetType](implicitly[Numeric[OffsetType]].plus(that, offset)) {
      override def encodeOffset: List[Byte] = NearPointer.this.encodeOffset
      override val operandByteSize: OperandSize = NearPointer.this.operandByteSize
    }
}

object ShortPointer {
  def apply(offset: X86Offset.ShortOffset) = new NearPointer(offset) {
    override val operandByteSize: ValueSize = ValueSize.Byte
    override def encodeOffset: List[Byte] = offset.encodeLittleEndian
  }
}

object LongPointer {
  def apply(offset: X86Offset.RealLongOffset) = new NearPointer(offset) {
    override val operandByteSize: ValueSize = ValueSize.Word
    override def encodeOffset: List[Byte] = offset.encodeLittleEndian
  }

  def apply(offset: X86Offset.ProtectedLongOffset) = new NearPointer(offset) {
    override val operandByteSize: ValueSize = ValueSize.DoubleWord
    override def encodeOffset: List[Byte] = offset.encodeLittleEndian
  }
}