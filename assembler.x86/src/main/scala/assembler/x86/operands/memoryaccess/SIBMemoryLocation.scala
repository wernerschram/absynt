package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.ProcessorMode
import assembler.x86.operands.{ModRMEncodableOperand, _}

//TODO: of Page 2-7 of the intel software development manual (325383-sdm-vol-2abcd.pdf) Notes: 1. and Scaled Index == none are not implemented
sealed class SIBMemoryLocation(val index: SIBIndexRegister, val base: SIBBaseRegister,
  displacement: Option[ImmediateValue] = None, val scale: Int, segment: SegmentRegister)
  extends IndirectMemoryLocation(0x04, displacement, index.operandByteSize, segment) with ModRMEncodableOperand {

  assume(index.operandByteSize == base.operandByteSize)
  assume((1 :: 2 :: 4 :: 8 :: Nil).contains(scale))

  override val defaultSegment: SegmentRegister = index.defaultSIBSegment
  val baseCode: Byte = base.SIBBaseCode
  val indexCode: Byte = index.SIBIndexCode

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

  def apply(index: SIBIndexRegister, base: SIBBaseRegister, scale: Int) =
    new SIBMemoryLocation(index, base, None, scale, index.defaultSIBSegment)

  def apply(index: SIBIndexRegister, base: SIBBaseRegister, displacement: ImmediateValue, scale: Int) =
    new SIBMemoryLocation(index, base, Some(displacement), scale, index.defaultSIBSegment)

  def withSize(index: SIBIndexRegister, base: SIBBaseRegister, scale: Int)(size: ValueSize): SIBMemoryLocation with FixedSizeOperand =
    new SIBMemoryLocation(index, base, None, scale, index.defaultSIBSegment) with FixedSizeOperand {
      override val operandByteSize: OperandSize = size
      override def toString = s"$operandByteSize PTR ${super.toString}"
    }

  def withSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: ImmediateValue, scale: Int)(size: ValueSize): SIBMemoryLocation with FixedSizeOperand =
    new SIBMemoryLocation(index, base, Some(displacement), scale, index.defaultSIBSegment) with FixedSizeOperand {
      override val operandByteSize: OperandSize = size
      override def toString = s"$operandByteSize PTR ${super.toString}"
    }

  object withSegmentOverride {
    def apply(index: SIBIndexRegister, base: SIBBaseRegister, scale: Int,
              segment: SegmentRegister) =
      new SIBMemoryLocation(index, base, None, scale, segment)

    def apply(index: SIBIndexRegister, base: SIBBaseRegister, displacement: ImmediateValue, scale: Int,
              segment: SegmentRegister) =
      new SIBMemoryLocation(index, base, Some(displacement), scale, segment)

    def withSize(index: SIBIndexRegister, base: SIBBaseRegister, scale: Int,
      segment: SegmentRegister)(size: ValueSize): SIBMemoryLocation with FixedSizeOperand =
        new SIBMemoryLocation(index, base, None, scale, segment) with FixedSizeOperand {
      override val operandByteSize: OperandSize = size
      override def toString = s"$operandByteSize PTR ${super.toString}"
    }

    def withSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: ImmediateValue, scale: Int,
      segment: SegmentRegister)(size: ValueSize): SIBMemoryLocation with FixedSizeOperand =
        new SIBMemoryLocation(index, base, Some(displacement), scale, segment) with FixedSizeOperand {
      override val operandByteSize: OperandSize = size
      override def toString = s"$operandByteSize PTR ${super.toString}"
    }
  }
}