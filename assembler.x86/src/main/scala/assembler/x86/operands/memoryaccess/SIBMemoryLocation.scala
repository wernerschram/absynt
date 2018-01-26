package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.operands.{ModRMEncodableOperand, _}
import assembler.x86.{ParameterPosition, ProcessorMode, RexRequirement}

sealed class SIBMemoryLocation(val index: SIBIndexRegister, val base: SIBBaseRegister, displacement: Displacement = Displacement.None, val scale: Int,
                               segment: SegmentRegister)
  extends IndirectMemoryLocation(0x04, displacement, index.operandByteSize, segment) with ModRMEncodableOperand {
  assume(index.operandByteSize == base.operandByteSize)
  assume((1 :: 2 :: 4 :: 8 :: Nil).contains(scale))

  override val defaultSegment: SegmentRegister = index.defaultSIBSegment
  val baseCode: Byte = base.SIBBaseCode
  val indexCode: Byte = index.SIBIndexCode

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = super.getExtendedBytes(rValue) ++ (getSIB +: displacement.encode)

  def getSIB: Byte = {
    val scaleCode = scale match {
      case 1 => 0x0
      case 2 => 0x1
      case 4 => 0x2
      case 8 => 0x3
    }
    ((scaleCode << 6) | (indexCode << 3) | baseCode).toByte
  }

  override def getRexRequirements(position: ParameterPosition): Seq[RexRequirement] =
    super.getRexRequirements(position) ++
      base.getRexRequirements(ParameterPosition.Base) ++
      index.getRexRequirements(ParameterPosition.Index)

  override def isValidForMode(processorMode: ProcessorMode): Boolean =
    base.isValidForMode(processorMode) && index.isValidForMode(processorMode)

  override def toString = s"$segmentPrefix[$base+$index$scaleString$displacementString]"

  private def scaleString = s"*$scale"

  private def displacementString = if (displacement == Displacement.None) "" else s"+${displacement.encode.decimalString}"
}

object SIBMemoryLocation {

  def apply(index: SIBIndexRegister, base: SIBBaseRegister, displacement: Displacement = Displacement.None, scale: Int = 1) =
    new SIBMemoryLocation(index, base, displacement, scale, index.defaultSIBSegment)

  def withSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: Displacement = Displacement.None, scale: Int = 1)(size: ValueSize): SIBMemoryLocation with FixedSizeOperand =
    new SIBMemoryLocation(index, base, displacement, scale, index.defaultSIBSegment) with FixedSizeOperand {
      override val operandByteSize: OperandSize = size
    }

  object withSegmentOverride {
    def apply(index: SIBIndexRegister, base: SIBBaseRegister, displacement: Displacement = Displacement.None, scale: Int = 1,
              segment: SegmentRegister) =
      new SIBMemoryLocation(index, base, displacement, scale, segment)

    def withSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: Displacement = Displacement.None, scale: Int = 1,
      segment: SegmentRegister)(size: ValueSize): SIBMemoryLocation with FixedSizeOperand =
        new SIBMemoryLocation(index, base, displacement, scale, segment) with FixedSizeOperand {
      override val operandByteSize: OperandSize = size
    }
  }
}