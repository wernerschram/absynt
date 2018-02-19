package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operations.AddressOperandInfo

import scala.language.implicitConversions

sealed class RegisterMemoryLocation[T <: RegisterReference] private(val reference: T, displacement: Option[ImmediateValue], segment: SegmentRegister)
  extends IndirectMemoryLocation(reference.indexCode,
     if (reference.onlyWithDisplacement)
      Some(displacement.getOrElse(ImmediateValue(0.toByte)))
    else
      displacement, segment)
    with ModRMEncodableOperand {

  override val addressOperands: Seq[AddressOperandInfo] = reference match {
    case bi: BaseIndexReference => Seq(AddressOperandInfo.rmBase(bi.base), AddressOperandInfo.rmIndex(bi.index))
    case o: GeneralPurposeRegister with IndexRegister => Seq(AddressOperandInfo.rmIndex(o))
  }

  override val defaultSegment: SegmentRegister = reference.defaultSegment

  override def toString: String = s"$segmentPrefix[$reference$displacementString]"

  private def displacementString = displacement match {
    case None => ""
    case Some(d) => s"+${d.value.decimalString}"
  }

  val actualDisplacement: Seq[Byte] =
    if (reference.onlyWithDisplacement)
      displacement.map(_.value).getOrElse(Seq(0.toByte))
    else
      displacement.toSeq.flatMap(_.value)

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

  def apply[T<:RegisterReference](index: T, displacement: ImmediateValue)=
    new RegisterMemoryLocation(index, Some(displacement), index.defaultSegment)

  def withSize(index: RegisterReference)(size: ValueSize): RegisterMemoryLocation[RegisterReference] with FixedSizeOperand =
    new RegisterMemoryLocation[RegisterReference](index, None, index.defaultSegment) with FixedSizeOperand {
      override val operandByteSize: OperandSize = size

      override def toString = s"$operandByteSize PTR ${super.toString()}"
    }

  def withSize(index: RegisterReference, displacement: ImmediateValue)(size: ValueSize): RegisterMemoryLocation[RegisterReference] with FixedSizeOperand =
    new RegisterMemoryLocation[RegisterReference](index, Some(displacement), index.defaultSegment) with FixedSizeOperand {
      override val operandByteSize: OperandSize = size

      override def toString = s"$operandByteSize PTR ${super.toString()}"
    }

  object withSegmentOverride {
    def apply[T<:RegisterReference](index: T, segment: SegmentRegister) =
      new RegisterMemoryLocation(index, None, segment)

    def apply[T<:RegisterReference](index: T, displacement: ImmediateValue, segment: SegmentRegister) =
      new RegisterMemoryLocation(index, Some(displacement), segment)

    def withSize(index: RegisterReference, segment: SegmentRegister)(size: ValueSize): RegisterMemoryLocation[RegisterReference] with FixedSizeOperand =
      new RegisterMemoryLocation[RegisterReference](index, None, segment) with FixedSizeOperand {
        override val operandByteSize: OperandSize = size

        override def toString = s"$operandByteSize PTR ${super.toString()}"
      }

    def withSize(index: RegisterReference, displacement: ImmediateValue, segment: SegmentRegister)(size: ValueSize): RegisterMemoryLocation[RegisterReference] with FixedSizeOperand =
      new RegisterMemoryLocation[RegisterReference](index, Some(displacement), segment) with FixedSizeOperand {
        override val operandByteSize: OperandSize = size

        override def toString = s"$operandByteSize PTR ${super.toString()}"
      }
  }

  implicit def indexWrapper(index: DestinationIndex with IndexRegister): RegisterMemoryLocation[DestinationIndex with IndexRegister] =
    new RegisterMemoryLocation[DestinationIndex with IndexRegister](index, None, index.defaultSegment)
}