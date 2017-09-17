package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.operands.{ModRMEncodableOperand, _}
import assembler.x86.{ParameterPosition, ProcessorMode, RexRequirement}

sealed class SIBMemoryLocation(val index: SIBIndexRegister, val base: SIBBaseRegister, displacement: List[Byte], val scale: Int,
                               segment: SegmentRegister)
  extends IndirectMemoryLocation(0x04, displacement, index.operandByteSize, segment) with ModRMEncodableOperand {
  assume(index.operandByteSize == base.operandByteSize)
  assume((1 :: 2 :: 4 :: 8 :: Nil).contains(scale))

  override val defaultSegment: SegmentRegister = index.defaultSIBSegment
  val baseCode: Byte = base.SIBBaseCode
  val indexCode: Byte = index.SIBIndexCode

  override def getExtendedBytes(rValue: Byte): List[Byte] = super.getExtendedBytes(rValue) ::: getSIB :: displacement

  def getSIB: Byte = {
    val scaleCode = scale match {
      case 1 => 0x0
      case 2 => 0x1
      case 4 => 0x2
      case 8 => 0x3
    }
    ((scaleCode << 6) | (indexCode << 3) | baseCode).toByte
  }

  override def getRexRequirements(position: ParameterPosition): List[RexRequirement] =
    super.getRexRequirements(position) :::
      base.getRexRequirements(ParameterPosition.Base) :::
      index.getRexRequirements(ParameterPosition.Index)

  override def isValidForMode(processorMode: ProcessorMode): Boolean =
    base.isValidForMode(processorMode) && index.isValidForMode(processorMode)

  override def toString = s"$segmentPrefix[$base+$index$scaleString$displacementString]"

  private def scaleString = s"*$scale"

  private def displacementString = if (displacement == Nil) "" else s"+${displacement.decimalString}"
}

object SIBMemoryLocation {

  def apply(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1) =
    new SIBMemoryLocation(index, base, displacement, scale, index.defaultSIBSegment)

  def byteSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1) =
    FixedSizeSIBMemoryLocation(index, base, displacement, scale, ValueSize.Byte, index.defaultSIBSegment)

  def wordSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1) =
    FixedSizeSIBMemoryLocation(index, base, displacement, scale, ValueSize.Word, index.defaultSIBSegment)

  def doubleWordSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1) =
    FixedSizeSIBMemoryLocation(index, base, displacement, scale, ValueSize.DoubleWord, index.defaultSIBSegment)

  def quadWordSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1) =
    FixedSizeSIBMemoryLocation(index, base, displacement, scale, ValueSize.QuadWord, index.defaultSIBSegment)

  final class FixedSizeSIBMemoryLocation private(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte], scale: Int,
                                                 val operandByteSize: OperandSize, segment: SegmentRegister)
    extends SIBMemoryLocation(index, base, displacement, scale, segment) with ModRMEncodableOperand with FixedSizeOperand

  object withSegmentOverride {
    def apply(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1,
              segment: SegmentRegister) =
      new SIBMemoryLocation(index, base, displacement, scale, segment)

    def byteSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1,
                 segment: SegmentRegister) =
      FixedSizeSIBMemoryLocation(index, base, displacement, scale, ValueSize.Byte, segment)

    def wordSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1,
                 segment: SegmentRegister) =
      FixedSizeSIBMemoryLocation(index, base, displacement, scale, ValueSize.Word, segment)

    def doubleWordSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1,
                       segment: SegmentRegister) =
      FixedSizeSIBMemoryLocation(index, base, displacement, scale, ValueSize.DoubleWord, segment)

    def quadWordSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1,
                     segment: SegmentRegister) =
      FixedSizeSIBMemoryLocation(index, base, displacement, scale, ValueSize.QuadWord, segment)

  }

  private object FixedSizeSIBMemoryLocation {
    def apply(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte], scale: Int, operandByteSize: OperandSize,
              segment: SegmentRegister) =
      new FixedSizeSIBMemoryLocation(index, base, displacement, scale, operandByteSize, segment)
  }

}