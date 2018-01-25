package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.operands._
import assembler.x86.{ParameterPosition, ProcessorMode, RexRequirement}

import scala.language.implicitConversions

sealed class RegisterMemoryLocation private(val index: BaseIndexPair, displacement: Displacement, segment: SegmentRegister)
  extends IndirectMemoryLocation(index.indexCode,
    if (displacement == Displacement.None && index.onlyWithDisplacement) Displacement(0.toByte) else displacement, index.operandByteSize, segment)
    with ModRMEncodableOperand {

  override val defaultSegment: SegmentRegister = index.defaultSegment

  override def toString: String = s"$segmentPrefix[$index$displacementString]"

  private def displacementString = if (displacement == Displacement.None) "" else s"+${displacement.encode.decimalString}"

  val actualDisplacement: Seq[Byte] =
    if (displacement == Displacement.None && index.onlyWithDisplacement) Seq(0.toByte) else displacement.encode

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = super.getExtendedBytes(rValue) ++ actualDisplacement

  override def getRexRequirements(position: ParameterPosition): Seq[RexRequirement] =
    index.getRexRequirements(ParameterPosition.OperandRM) ++ super.getRexRequirements(position)

  override def isValidForMode(processorMode: ProcessorMode): Boolean = (index, processorMode) match {
    case (_: BaseIndexPair, ProcessorMode.Real | ProcessorMode.Protected) => true
    case (_: ProtectedModeIndexRegister, _) => true
    case _ => false
  }
}

object RegisterMemoryLocation {

  def apply(index: BaseIndexPair, displacement: Displacement = Displacement.None)=
    new RegisterMemoryLocation(index, displacement, index.defaultSegment)

  def apply(index: DestinationIndex, displacement: Displacement, segment: SegmentRegister) =
    new DIReference(index, displacement, segment)

  def byteSize(index: BaseIndexPair, displacement: Displacement = Displacement.None)=
    new FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.Byte, index.defaultSegment)

  def wordSize(index: BaseIndexPair, displacement: Displacement = Displacement.None)=
    new FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.Word, index.defaultSegment)

  def doubleWordSize(index: BaseIndexPair, displacement: Displacement = Displacement.None)=
    new FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.DoubleWord, index.defaultSegment)

  def quadWordSize(index: BaseIndexPair, displacement: Displacement = Displacement.None)=
    new FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.QuadWord, index.defaultSegment)

  final class DIReference private[RegisterMemoryLocation](index: DestinationIndex, displacement: Displacement, segment: SegmentRegister)
    extends RegisterMemoryLocation(index, displacement, segment)

  final class FixedSizeRegisterMemoryLocation private[RegisterMemoryLocation](index: BaseIndexPair, displacement: Displacement,
                                                      override val operandByteSize: OperandSize, segment: SegmentRegister)
    extends RegisterMemoryLocation(index, displacement, segment) with ModRMEncodableOperand with FixedSizeOperand {

    override def toString = s"$operandByteSize PTR ${super.toString()}"
  }

  object withSegmentOverride {
    def apply(index: BaseIndexPair, displacement: Displacement = Displacement.None, segment: SegmentRegister) =
      new RegisterMemoryLocation(index, displacement, segment)

    def byteSize(index: BaseIndexPair, displacement: Displacement = Displacement.None, segment: SegmentRegister) =
      FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.Byte, segment)

    def wordSize(index: BaseIndexPair, displacement: Displacement = Displacement.None, segment: SegmentRegister) =
      FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.Word, segment)

    def doubleWordSize(index: BaseIndexPair, displacement: Displacement = Displacement.None, segment: SegmentRegister) =
      FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.DoubleWord, segment)

    def quadWordSize(index: BaseIndexPair, displacement: Displacement = Displacement.None, segment: SegmentRegister) =
      FixedSizeRegisterMemoryLocation(index, displacement, ValueSize.QuadWord, segment)
  }

  private object FixedSizeRegisterMemoryLocation {
    def apply(index: BaseIndexPair, displacement: Displacement, operandByteSize: OperandSize, segment: SegmentRegister) =
      new FixedSizeRegisterMemoryLocation(index, displacement, operandByteSize, segment)
  }

  def apply(index: DestinationIndex) =
    new DIReference(index, Displacement.None, index.defaultSegment)

  implicit def indexWrapper(index: DestinationIndex): DIReference = apply(index)


}