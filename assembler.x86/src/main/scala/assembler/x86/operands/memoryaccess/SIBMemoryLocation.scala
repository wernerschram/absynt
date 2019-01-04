package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.ProcessorMode
import assembler.x86.operands.{ModRMEncodableOperand, _}
import assembler.x86.operations.AddressOperandInfo

//TODO: of Page 2-7 of the intel software development manual (325383-sdm-vol-2abcd.pdf) Notes: 1. and Scaled Index == none are not implemented
sealed class SIBMemoryLocation(val index: GeneralPurposeRegister with SIBIndexRegister, val base: GeneralPurposeRegister with SIBBaseRegister,
  displacement: Option[ImmediateValue with DisplacementSize] = None, val scale: Int, segment: SegmentRegister)
  extends IndirectMemoryLocation(0x04, displacement, segment) with ModRMEncodableOperand {

  assume(index sizeEquals  base)
  assume((1 :: 2 :: 4 :: 8 :: Nil).contains(scale))

  override val defaultSegment: SegmentRegister = index.defaultSIBSegment
  val baseCode: Byte = base.SIBBaseCode
  val indexCode: Byte = index.SIBIndexCode

  override val addressOperands: Set[AddressOperandInfo] = Set(AddressOperandInfo.SIBBase(base), AddressOperandInfo.SIBIndex(index, segmentOverride))

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = super.getExtendedBytes(rValue) ++ (getSIB +: displacement.toSeq.flatMap(_.value))

  def getSIB: Byte = {
    val scaleCode = scale match {
      case 1 => 0x0
      case 2 => 0x1
      case 4 => 0x2
      case 8 => 0x3
    }
    ((scaleCode << 6) | (indexCode << 3) | baseCode).toByte
  }

  override def isValidForMode(processorMode: ProcessorMode): Boolean =
    base.isValidForMode(processorMode) && index.isValidForMode(processorMode)

  override def toString = s"$segmentPrefix[$base+$index$scaleString$displacementString]"

  private def scaleString = s"*$scale"

  private def displacementString = displacement match {
    case None => ""
    case Some(d) => s"+${d.value.decimalString}"
  }
}

object SIBMemoryLocation {
    abstract class SIBForSize[Size<:ValueSize]() {
    def instance(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, displacement: Option[ImmediateValue with DisplacementSize], scale: Int, segmentOverride: SegmentRegister): SIBMemoryLocation with Size
  }

  implicit def SIBforByteSize: SIBForSize[ByteSize] =
    (index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, displacement: Option[ImmediateValue with DisplacementSize], scale: Int, segmentOverride: SegmentRegister) =>
      new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with ByteSize

  implicit def SIBforWordSize: SIBForSize[WordSize] =
    (index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, displacement: Option[ImmediateValue with DisplacementSize], scale: Int, segmentOverride: SegmentRegister) =>
      new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with WordSize

  implicit def SIBforDoubleWordSize: SIBForSize[DoubleWordSize] =
    (index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, displacement: Option[ImmediateValue with DisplacementSize], scale: Int, segmentOverride: SegmentRegister) =>
      new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with DoubleWordSize

  implicit def SIBforQuadWordSize: SIBForSize[QuadWordSize] =
    (index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, displacement: Option[ImmediateValue with DisplacementSize], scale: Int, segmentOverride: SegmentRegister) =>
      new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with QuadWordSize

  def apply[Size<:ValueSize: SIBForSize](index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, scale: Int): SIBMemoryLocation with Size =
    implicitly[SIBForSize[Size]].instance(index, base, None, scale, index.defaultSIBSegment)

  def apply[Size<:ValueSize: SIBForSize](index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, displacement: ImmediateValue with DisplacementSize, scale: Int): SIBMemoryLocation with Size =
    implicitly[SIBForSize[Size]].instance(index, base, Some(displacement), scale, index.defaultSIBSegment)

  object withSegmentOverride {
    def apply[Size<:ValueSize: SIBForSize](index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, scale: Int, segment: SegmentRegister): SIBMemoryLocation with Size =
      implicitly[SIBForSize[Size]].instance(index, base, None, scale, segment)

    def apply[Size<:ValueSize: SIBForSize](index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, displacement: ImmediateValue with DisplacementSize, scale: Int, segment: SegmentRegister): SIBMemoryLocation with Size =
      implicitly[SIBForSize[Size]].instance(index, base, Some(displacement), scale, segment)
  }
}