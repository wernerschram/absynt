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

  def apply(index: DestinationIndex, displacement: Displacement) =
    new DIReference(index, displacement, index.defaultSegment)

  def withSize(index: BaseIndexPair, displacement: Displacement = Displacement.None)(size: ValueSize): RegisterMemoryLocation with FixedSizeOperand =
    new RegisterMemoryLocation(index, displacement, index.defaultSegment) with FixedSizeOperand {
      override val operandByteSize: OperandSize = size

      override def toString = s"$operandByteSize PTR ${super.toString()}"
    }

  // TODO: Restrictions on types with generics look nicer
  final class DIReference private[RegisterMemoryLocation](index: DestinationIndex, displacement: Displacement, segment: SegmentRegister)
    extends RegisterMemoryLocation(index, displacement, segment)

  object withSegmentOverride {
    def apply(index: BaseIndexPair, displacement: Displacement = Displacement.None, segment: SegmentRegister) =
      new RegisterMemoryLocation(index, displacement, segment)

  def apply(index: DestinationIndex, displacement: Displacement, segment: SegmentRegister) =
    new DIReference(index, displacement, segment)

    def withSize(index: BaseIndexPair, displacement: Displacement = Displacement.None, segment: SegmentRegister)(size: ValueSize): RegisterMemoryLocation with FixedSizeOperand =
      new RegisterMemoryLocation(index, displacement, segment) with FixedSizeOperand {
        override val operandByteSize: OperandSize = size

        override def toString = s"$operandByteSize PTR ${super.toString()}"
      }
  }

  def apply(index: DestinationIndex) =
    new DIReference(index, Displacement.None, index.defaultSegment)

  implicit def indexWrapper(index: DestinationIndex): DIReference = apply(index)


}