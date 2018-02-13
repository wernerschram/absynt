package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.operands._
import assembler.x86.{ParameterPosition, ProcessorMode, RexRequirement}

import scala.language.implicitConversions

sealed class RegisterMemoryLocation[T <: RegisterReference] private(val index: T, displacement: Displacement, segment: SegmentRegister)
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
    case (_: RegisterReference, ProcessorMode.Real | ProcessorMode.Protected) => true
    case (_: ProtectedModeIndexRegister, _) => true
    case _ => false
  }
}

object RegisterMemoryLocation {

  def apply[T<:RegisterReference](index: T, displacement: Displacement = Displacement.None)=
    new RegisterMemoryLocation(index, displacement, index.defaultSegment)

  def withSize(index: RegisterReference, displacement: Displacement = Displacement.None)(size: ValueSize): RegisterMemoryLocation[RegisterReference] with FixedSizeOperand =
    new RegisterMemoryLocation[RegisterReference](index, displacement, index.defaultSegment) with FixedSizeOperand {
      override val operandByteSize: OperandSize = size

      override def toString = s"$operandByteSize PTR ${super.toString()}"
    }

  object withSegmentOverride {
    def apply[T<:RegisterReference](index: T, displacement: Displacement = Displacement.None, segment: SegmentRegister) =
      new RegisterMemoryLocation(index, displacement, segment)

    def withSize(index: RegisterReference, displacement: Displacement = Displacement.None, segment: SegmentRegister)(size: ValueSize): RegisterMemoryLocation[RegisterReference] with FixedSizeOperand =
      new RegisterMemoryLocation[RegisterReference](index, displacement, segment) with FixedSizeOperand {
        override val operandByteSize: OperandSize = size

        override def toString = s"$operandByteSize PTR ${super.toString()}"
      }
  }

  implicit def indexWrapper(index: DestinationIndex with IndexRegister): RegisterMemoryLocation[DestinationIndex with IndexRegister] =
    new RegisterMemoryLocation[DestinationIndex with IndexRegister](index, Displacement.None, index.defaultSegment)
}