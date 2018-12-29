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
  def apply(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, scale: Int) =
    new SIBMemoryLocation(index, base, None, scale, index.defaultSIBSegment)

  def apply(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, displacement: ImmediateValue with DisplacementSize, scale: Int) =
    new SIBMemoryLocation(index, base, Some(displacement), scale, index.defaultSIBSegment)

  def byteSize(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, scale: Int) =
    new SIBMemoryLocation(index, base, None, scale, index.defaultSIBSegment) with ByteSize

  def byteSize(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, displacement: ImmediateValue with DisplacementSize, scale: Int) =
    new SIBMemoryLocation(index, base, Some(displacement), scale, index.defaultSIBSegment) with ByteSize

  def wordSize(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, scale: Int) =
    new SIBMemoryLocation(index, base, None, scale, index.defaultSIBSegment) with WordSize

  def wordSize(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, displacement: ImmediateValue with DisplacementSize, scale: Int) =
    new SIBMemoryLocation(index, base, Some(displacement), scale, index.defaultSIBSegment) with WordSize

  def doubleWordSize(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, scale: Int) =
    new SIBMemoryLocation(index, base, None, scale, index.defaultSIBSegment) with DoubleWordSize

  def doubleWordSize(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, displacement: ImmediateValue with DisplacementSize, scale: Int) =
    new SIBMemoryLocation(index, base, Some(displacement), scale, index.defaultSIBSegment) with DoubleWordSize

  def quadWordSize(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, scale: Int) =
    new SIBMemoryLocation(index, base, None, scale, index.defaultSIBSegment) with QuadWordSize

  def quadWordSize(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, displacement: ImmediateValue with DisplacementSize, scale: Int) =
    new SIBMemoryLocation(index, base, Some(displacement), scale, index.defaultSIBSegment) with QuadWordSize

  object withSegmentOverride {
    def apply(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, scale: Int,
              segment: SegmentRegister) =
      new SIBMemoryLocation(index, base, None, scale, segment)

    def apply(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, displacement: ImmediateValue with DisplacementSize, scale: Int,
              segment: SegmentRegister) =
      new SIBMemoryLocation(index, base, Some(displacement), scale, segment)

    def byteSize(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, scale: Int, segment: SegmentRegister) =
      new SIBMemoryLocation(index, base, None, scale, segment) with ByteSize

    def byteSize(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, displacement: ImmediateValue with DisplacementSize, scale: Int, segment: SegmentRegister) =
      new SIBMemoryLocation(index, base, Some(displacement), scale, segment) with ByteSize

    def wordSize(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, scale: Int, segment: SegmentRegister) =
      new SIBMemoryLocation(index, base, None, scale, segment) with WordSize

    def wordSize(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, displacement: ImmediateValue with DisplacementSize, scale: Int, segment: SegmentRegister) =
      new SIBMemoryLocation(index, base, Some(displacement), scale, segment) with WordSize

    def doubleWordSize(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, scale: Int, segment: SegmentRegister) =
      new SIBMemoryLocation(index, base, None, scale, segment) with DoubleWordSize

    def doubleWordSize(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, displacement: ImmediateValue with DisplacementSize, scale: Int, segment: SegmentRegister) =
      new SIBMemoryLocation(index, base, Some(displacement), scale, segment) with DoubleWordSize

    def quadWordSize(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, scale: Int, segment: SegmentRegister) =
      new SIBMemoryLocation(index, base, None, scale, segment) with QuadWordSize

    def quadWordSize(index: GeneralPurposeRegister with SIBIndexRegister, base: GeneralPurposeRegister with SIBBaseRegister, displacement: ImmediateValue with DisplacementSize, scale: Int, segment: SegmentRegister) =
      new SIBMemoryLocation(index, base, Some(displacement), scale, segment) with QuadWordSize
  }
}