package assembler.x86.operands.memoryaccess

import scala.language.implicitConversions

import assembler.ListExtensions._
import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.RexExtendedRequirement
import assembler.x86.operands.FixedSizeModRMEncodableOperand
import assembler.x86.operands._

sealed class RegisterMemoryLocation private (val index: BaseIndexPair, displacement: List[Byte], segment: SegmentRegister)
    extends IndirectMemoryLocation(index.indexCode, displacement, index.operandByteSize, segment)
    with ModRMEncodableOperand {

  override def toString(): String = s"${segment.getSegmentPrefix(defaultSegment)}[${index}${displacementString}]"

  private def displacementString = if (displacement == Nil) "" else s"+${displacement.decimalString()}"

  override val defaultSegment: SegmentRegister = index.defaultSegment

  override def getExtendedBytes(rValue: Byte): List[Byte] = super.getExtendedBytes(rValue) ::: displacement

  override def getRexRequirements(position: ParameterPosition): List[RexExtendedRequirement] =
    index.getRexRequirements(ParameterPosition.OperandRM)

  override def isValidForMode(processorMode: ProcessorMode): Boolean = (index, processorMode) match {
    case (realIndex: BaseIndexPair, ProcessorMode.Real | ProcessorMode.Protected) => true
    case (protectedIndex: ProtectedModeIndexRegister, _) => true
    case _ => false
  }
}

object RegisterMemoryLocation {
  final class DIReference private[RegisterMemoryLocation] (index: DestinationIndex, displacement: List[Byte], segment: SegmentRegister)
    extends RegisterMemoryLocation(index, displacement, segment)

  final class FixedSizeRegisterMemoryLocation private (
    index: BaseIndexPair, displacement: List[Byte], override val operandByteSize: Int, segment: SegmentRegister)
      extends RegisterMemoryLocation(index, displacement, segment) with FixedSizeModRMEncodableOperand {
    def sizeString = operandByteSize match {
      case 1 => "BYTE"
      case 2 => "WORD"
      case 4 => "DWORD"
      case 8 => "QWORD"
      case default => throw new AssertionError
    }

    override def toString = s"${sizeString} PTR ${super.toString()}"
  }

  private object FixedSizeRegisterMemoryLocation {
    def apply(index: BaseIndexPair, displacement: List[Byte], operandByteSize: Int, segment: SegmentRegister) =
      new FixedSizeRegisterMemoryLocation(index, displacement, operandByteSize, segment)
  }

  def apply(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte]) =
    new RegisterMemoryLocation(index, displacement, index.defaultSegment)

  def apply(index: DestinationIndex, displacement: List[Byte], segment: SegmentRegister) =
    new DIReference(index, displacement, segment)

  def apply(index: DestinationIndex) =
    new DIReference(index, List.empty[Byte], index.defaultSegment)

  implicit def indexWrapper(index: DestinationIndex) = apply(index)

  object withSegmentOverride {
    def apply(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte], segment: SegmentRegister) =
      new RegisterMemoryLocation(index, displacement, segment)

    def byteSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte], segment: SegmentRegister) =
      FixedSizeRegisterMemoryLocation(index, displacement, 1, segment)

    def wordSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte], segment: SegmentRegister) =
      FixedSizeRegisterMemoryLocation(index, displacement, 2, segment)

    def doubleWordSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte], segment: SegmentRegister) =
      FixedSizeRegisterMemoryLocation(index, displacement, 4, segment)

    def quadWordSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte], segment: SegmentRegister) =
      FixedSizeRegisterMemoryLocation(index, displacement, 8, segment)
  }

  def byteSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte]) =
    FixedSizeRegisterMemoryLocation(index, displacement, 1, index.defaultSegment)

  def wordSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte]) =
    FixedSizeRegisterMemoryLocation(index, displacement, 2, index.defaultSegment)

  def doubleWordSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte]) =
    FixedSizeRegisterMemoryLocation(index, displacement, 4, index.defaultSegment)

  def quadWordSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte]) =
    FixedSizeRegisterMemoryLocation(index, displacement, 8, index.defaultSegment)
}