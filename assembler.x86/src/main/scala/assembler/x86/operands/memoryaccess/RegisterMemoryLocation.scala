package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.operands._
import assembler.x86.{ParameterPosition, ProcessorMode, RexRequirement}

import scala.language.implicitConversions

sealed class RegisterMemoryLocation private(val index: BaseIndexPair, displacement: List[Byte], segment: SegmentRegister)
  extends IndirectMemoryLocation(index.indexCode, displacement, index.operandByteSize, segment)
    with ModRMEncodableOperand {

  override val defaultSegment: SegmentRegister = index.defaultSegment

  override def toString: String = s"$segmentPrefix[$index$displacementString]"

  private def displacementString = if (displacement == Nil) "" else s"+${displacement.decimalString}"

  override def getExtendedBytes(rValue: Byte): List[Byte] = super.getExtendedBytes(rValue) ::: displacement

  override def getRexRequirements(position: ParameterPosition): List[RexRequirement] =
    index.getRexRequirements(ParameterPosition.OperandRM) ::: super.getRexRequirements(position)

  override def isValidForMode(processorMode: ProcessorMode): Boolean = (index, processorMode) match {
    case (_: BaseIndexPair, ProcessorMode.Real | ProcessorMode.Protected) => true
    case (_: ProtectedModeIndexRegister, _) => true
    case _ => false
  }
}

object RegisterMemoryLocation {

  def apply(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte]) =
    new RegisterMemoryLocation(index, displacement, index.defaultSegment)

  def apply(index: DestinationIndex, displacement: List[Byte], segment: SegmentRegister) =
    new DIReference(index, displacement, segment)

  def byteSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte]) =
    FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.Byte, index.defaultSegment)

  def wordSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte]) =
    FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.Word, index.defaultSegment)

  def doubleWordSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte]) =
    FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.DoubleWord, index.defaultSegment)

  def quadWordSize(index: BaseIndexPair, displacement: List[Byte] = List.empty[Byte]) =
    FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.QuadWord, index.defaultSegment)

  final class DIReference private[RegisterMemoryLocation](index: DestinationIndex, displacement: List[Byte], segment: SegmentRegister)
    extends RegisterMemoryLocation(index, displacement, segment)

  final class FixedSizeRegisterMemoryLocation private(index: BaseIndexPair, displacement: List[Byte],
                                                      override val operandByteSize: OperandSize, segment: SegmentRegister)
    extends RegisterMemoryLocation(index, displacement, segment) with ModRMEncodableOperand with FixedSizeOperand {

    override def toString = s"$operandByteSize PTR ${super.toString()}"
  }

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

  private object FixedSizeRegisterMemoryLocation {
    def apply(index: BaseIndexPair, displacement: List[Byte], operandByteSize: OperandSize, segment: SegmentRegister) =
      new FixedSizeRegisterMemoryLocation(index, displacement, operandByteSize, segment)
  }

  def apply(index: DestinationIndex) =
    new DIReference(index, List.empty[Byte], index.defaultSegment)

  implicit def indexWrapper(index: DestinationIndex): DIReference = apply(index)


}