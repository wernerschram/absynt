package assembler.x86.operations

import assembler.x86.HasOperandSizePrefixRequirements
import assembler.x86.operands.memoryaccess.{FarPointer => FarPointerType, MemoryLocation => MemoryLocationType, NearPointer => NearPointerType}
import assembler.x86.operands.{FarPointerSize, ModRMEncodableOperand, ValueSize, WordDoubleSize}
import assembler.x86.operations.OperandInfo.OperandOrder._

sealed trait DisplacementBytes {
  self: X86Operation =>

  def displacementBytes: Seq[Byte]

  protected override def allOperands: Set[OperandInfo[_]]
}

trait NoDisplacement extends DisplacementBytes {
  self: X86Operation =>
  override def displacementBytes: Seq[Byte] = Nil
}

trait ModRMDisplacement[Size<:ValueSize] extends DisplacementBytes {
  self: ModRM[Size] =>
  val operandRM: ModRMEncodableOperand with Size

  override def displacementBytes: Seq[Byte] = Nil
}

trait FarPointer[OffsetSize<:WordDoubleSize] extends DisplacementBytes {
  self: X86Operation with HasOperandSizePrefixRequirements =>

  def pointer: FarPointerType[OffsetSize] with FarPointerSize[OffsetSize]

  protected override abstract def allOperands: Set[OperandInfo[_]] =
    super.allOperands + OperandInfo.pointer(pointer, destination)

  override def displacementBytes: Seq[Byte] = pointer.encodeByte
}

trait NearPointer[Size<:ValueSize] extends DisplacementBytes {
  self: X86Operation with HasOperandSizePrefixRequirements =>

  def pointer: NearPointerType with Size
  def pointerOrder: OperandOrder

  override def displacementBytes: Seq[Byte] = pointer.encodeBytes

  protected override abstract def allOperands: Set[OperandInfo[_]] =
    super.allOperands + OperandInfo.relative(pointer, pointerOrder)
}

trait MemoryLocation[Size<:ValueSize] extends DisplacementBytes {
  self: X86Operation with HasOperandSizePrefixRequirements =>

  def location: MemoryLocationType with Size
  def offsetOrder: OperandOrder

  protected override abstract def allOperands: Set[OperandInfo[_]] =
    super.allOperands + OperandInfo.memoryOffset(location, offsetOrder)

  override def displacementBytes: Seq[Byte] = location.displacement.toSeq.flatMap(_.value)

  def addressOperands(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): Set[AddressOperandInfo] =
    location.addressOperands
}
