package assembler.x86.operands.memoryaccess

import scala.language.implicitConversions

import assembler.ListExtensions._
import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.RexRequirement
import assembler.x86.operands.FixedSizeModRMEncodableOperand
import assembler.x86.operands._

sealed class RegisterMemoryLocation private (val index: BaseIndexPair, displacement: List[Byte], segment: SegmentRegister)
    extends IndirectMemoryLocation(index.indexCode, displacement, index.operandByteSize, segment)
    with ModRMEncodableOperand {

  override def toString(): String = s"${segment.getSegmentPrefix(defaultSegment)}[${index}${displacementString}]"

  private def displacementString = if (displacement == Nil) "" else s"+${displacement.decimalString()}"

  override val defaultSegment: SegmentRegister = index.defaultSegment

  override def getExtendedBytes(rValue: Byte): List[Byte] = super.getExtendedBytes(rValue) ::: displacement

  override def getRexRequirements(position: ParameterPosition): List[RexRequirement] =
    index.getRexRequirements(ParameterPosition.OperandRM) ::: super.getRexRequirements(position)

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
    index: BaseIndexPair, displacement: List[Byte], override val operandByteSize: OperandSize, segment: SegmentRegister)
      extends RegisterMemoryLocation(index, displacement, segment) with FixedSizeModRMEncodableOperand {

    override def toString = s"${operandByteSize} PTR ${super.toString()}"
  }

  private object FixedSizeRegisterMemoryLocation {
    def apply(index: BaseIndexPair, displacement: List[Byte], operandByteSize: OperandSize, segment: SegmentRegister) =
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
      FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.Byte, segment)

    def wordSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte], segment: SegmentRegister) =
      FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.Word, segment)

    def doubleWordSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte], segment: SegmentRegister) =
      FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.DoubleWord, segment)

    def quadWordSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte], segment: SegmentRegister) =
      FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.QuadWord, segment)
  }

  def byteSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte]) =
    FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.Byte, index.defaultSegment)

  def wordSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte]) =
    FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.Word, index.defaultSegment)

  def doubleWordSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte]) =
    FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.DoubleWord, index.defaultSegment)

  def quadWordSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte]) =
    FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.QuadWord, index.defaultSegment)
}