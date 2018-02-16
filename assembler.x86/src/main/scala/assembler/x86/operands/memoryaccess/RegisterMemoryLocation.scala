package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.ProcessorMode
import assembler.x86.operands._

import scala.language.implicitConversions

sealed class RegisterMemoryLocation[T <: RegisterReference] private(val reference: T, displacement: Option[Displacement], segment: SegmentRegister)
  extends IndirectMemoryLocation(reference.indexCode,
     if (reference.onlyWithDisplacement)
      Some(displacement.getOrElse(Displacement(0.toByte)))
    else
      displacement
  , reference.operandByteSize, segment)
    with ModRMEncodableOperand {

  override val defaultSegment: SegmentRegister = reference.defaultSegment

  override def toString: String = s"$segmentPrefix[$reference$displacementString]"

  private def displacementString = displacement match {
    case None => ""
    case Some(d) => s"+${d.encode.decimalString}"
  }

  val actualDisplacement: Seq[Byte] =
    if (reference.onlyWithDisplacement)
      displacement.map(_.encode).getOrElse(Seq(0.toByte))
    else
      displacement.toSeq.flatMap(_.encode)

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = super.getExtendedBytes(rValue) ++ actualDisplacement

  override def isValidForMode(processorMode: ProcessorMode): Boolean = (reference, processorMode) match {
    case (_: RegisterReference, ProcessorMode.Real | ProcessorMode.Protected) => true
    case (_: ProtectedModeIndexRegister, _) => true
    case _ => false
  }
}

object RegisterMemoryLocation {

  def apply[T<:RegisterReference](index: T)=
    new RegisterMemoryLocation(index, None, index.defaultSegment)

  def apply[T<:RegisterReference](index: T, displacement: Displacement)=
    new RegisterMemoryLocation(index, Some(displacement), index.defaultSegment)

  def withSize(index: RegisterReference)(size: ValueSize): RegisterMemoryLocation[RegisterReference] with FixedSizeOperand =
    new RegisterMemoryLocation[RegisterReference](index, None, index.defaultSegment) with FixedSizeOperand {
      override val operandByteSize: OperandSize = size

      override def toString = s"$operandByteSize PTR ${super.toString()}"
    }

  def withSize(index: RegisterReference, displacement: Displacement)(size: ValueSize): RegisterMemoryLocation[RegisterReference] with FixedSizeOperand =
    new RegisterMemoryLocation[RegisterReference](index, Some(displacement), index.defaultSegment) with FixedSizeOperand {
      override val operandByteSize: OperandSize = size

      override def toString = s"$operandByteSize PTR ${super.toString()}"
    }

  object withSegmentOverride {
    def apply[T<:RegisterReference](index: T, segment: SegmentRegister) =
      new RegisterMemoryLocation(index, None, segment)

    def apply[T<:RegisterReference](index: T, displacement: Displacement , segment: SegmentRegister) =
      new RegisterMemoryLocation(index, Some(displacement), segment)

    def withSize(index: RegisterReference, segment: SegmentRegister)(size: ValueSize): RegisterMemoryLocation[RegisterReference] with FixedSizeOperand =
      new RegisterMemoryLocation[RegisterReference](index, None, segment) with FixedSizeOperand {
        override val operandByteSize: OperandSize = size

        override def toString = s"$operandByteSize PTR ${super.toString()}"
      }

    def withSize(index: RegisterReference, displacement: Displacement, segment: SegmentRegister)(size: ValueSize): RegisterMemoryLocation[RegisterReference] with FixedSizeOperand =
      new RegisterMemoryLocation[RegisterReference](index, Some(displacement), segment) with FixedSizeOperand {
        override val operandByteSize: OperandSize = size

        override def toString = s"$operandByteSize PTR ${super.toString()}"
      }
  }

  implicit def indexWrapper(index: DestinationIndex with IndexRegister): RegisterMemoryLocation[DestinationIndex with IndexRegister] =
    new RegisterMemoryLocation[DestinationIndex with IndexRegister](index, None, index.defaultSegment)
}