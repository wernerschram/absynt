package assembler.x86.operations

import assembler.x86.operands.memoryaccess.{FarPointer => FarPointerType, MemoryLocation => MemoryLocationType, NearPointer => NearPointerType}
import assembler.x86.operands.{ExtendedSize, FarPointerSize, ModRMEncodableOperand, ValueSize}
import assembler.x86.operations.OperandInfo.OperandOrder._

sealed trait DisplacementBytes {
  self: X86Operation =>
  def displacementBytes: Seq[Byte]
  private[operations] def displacementInit(): Unit
}

trait NoDisplacement extends DisplacementBytes {
  self: X86Operation =>
  override def displacementBytes: Seq[Byte] = Nil
  override final def displacementInit(): Unit = Unit
}

trait ModRMDisplacement[Size<:ValueSize] extends DisplacementBytes {
  self: ModRM[Size] =>
  val operandRM: ModRMEncodableOperand with Size

  override def displacementBytes: Seq[Byte] = Nil
  override final def displacementInit(): Unit = Unit
}

trait FarPointer[OffsetSize<:ExtendedSize] extends DisplacementBytes {

  self: X86Operation =>
  def pointer: FarPointerType[OffsetSize] with FarPointerSize[OffsetSize]

  override final def displacementInit(): Unit =
    addOperand(OperandInfo.pointer(pointer, destination))

  override def displacementBytes: Seq[Byte] = pointer.encodeByte
}

trait NearPointer[Size<:ValueSize] extends DisplacementBytes {

  self: X86Operation =>
  def pointer: NearPointerType with Size
  def pointerOrder: OperandOrder

  override def displacementBytes: Seq[Byte] = pointer.encodeBytes

  final def displacementInit(): Unit =
    addOperand(OperandInfo.relative(pointer, pointerOrder))
}

trait MemoryLocation[Size<:ValueSize] extends DisplacementBytes {

  self: X86Operation =>
  def location: MemoryLocationType with Size
  def offsetOrder: OperandOrder

  override final def displacementInit(): Unit =
    addOperand(OperandInfo.memoryOffset(location, offsetOrder))

  override def displacementBytes: Seq[Byte] = location.displacement.toSeq.flatMap(_.value)

  def addressOperands: Set[AddressOperandInfo] = location.addressOperands
}
