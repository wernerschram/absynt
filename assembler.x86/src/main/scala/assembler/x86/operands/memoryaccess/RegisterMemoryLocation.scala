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

  override val addressOperands: Set[AddressOperandInfo] = reference match {
    case bi: BaseIndexReference => Set(AddressOperandInfo.rmBase(bi.base), AddressOperandInfo.rmIndex(bi.index))
    case o: GeneralPurposeRegister with IndexRegister => Set(AddressOperandInfo.rmIndex(o))
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

  def byteSize[T<:RegisterReference](index: T)=
    new RegisterMemoryLocation(index, None, index.defaultSegment) with ByteSize

  def byteSize[T<:RegisterReference](index: T, displacement: ImmediateValue)=
    new RegisterMemoryLocation(index, Some(displacement), index.defaultSegment) with ByteSize

  def wordSize[T<:RegisterReference](index: T)=
    new RegisterMemoryLocation(index, None, index.defaultSegment) with WordSize

  def wordSize[T<:RegisterReference](index: T, displacement: ImmediateValue)=
    new RegisterMemoryLocation(index, Some(displacement), index.defaultSegment) with WordSize

  def doubleWordSize[T<:RegisterReference](index: T)=
    new RegisterMemoryLocation(index, None, index.defaultSegment) with DoubleWordSize

  def doubleWordSize[T<:RegisterReference](index: T, displacement: ImmediateValue)=
    new RegisterMemoryLocation(index, Some(displacement), index.defaultSegment) with DoubleWordSize

  def quadWordSize[T<:RegisterReference](index: T)=
    new RegisterMemoryLocation(index, None, index.defaultSegment) with QuadWordSize

  def quadWordSize[T<:RegisterReference](index: T, displacement: ImmediateValue)=
    new RegisterMemoryLocation(index, Some(displacement), index.defaultSegment) with QuadWordSize

  object withSegmentOverride {
    def apply[T<:RegisterReference](index: T, segment: SegmentRegister) =
      new RegisterMemoryLocation(index, None, segment)

    def apply[T<:RegisterReference](index: T, displacement: ImmediateValue, segment: SegmentRegister) =
      new RegisterMemoryLocation(index, Some(displacement), segment)

    def byteSize[T<:RegisterReference](index: T, segment: SegmentRegister)=
      new RegisterMemoryLocation(index, None, segment) with ByteSize

    def byteSize[T<:RegisterReference](index: T, displacement: ImmediateValue, segment: SegmentRegister)=
      new RegisterMemoryLocation(index, Some(displacement), segment) with ByteSize

    def wordSize[T<:RegisterReference](index: T, segment: SegmentRegister)=
      new RegisterMemoryLocation(index, None, segment) with WordSize

    def wordSize[T<:RegisterReference](index: T, displacement: ImmediateValue, segment: SegmentRegister)=
      new RegisterMemoryLocation(index, Some(displacement), segment) with WordSize

    def doubleWordSize[T<:RegisterReference](index: T, segment: SegmentRegister)=
      new RegisterMemoryLocation(index, None, segment) with DoubleWordSize

    def doubleWordSize[T<:RegisterReference](index: T, displacement: ImmediateValue, segment: SegmentRegister)=
      new RegisterMemoryLocation(index, Some(displacement), segment) with DoubleWordSize

    def quadWordSize[T<:RegisterReference](index: T, segment: SegmentRegister)=
      new RegisterMemoryLocation(index, None, segment) with QuadWordSize

    def quadWordSize[T<:RegisterReference](index: T, displacement: ImmediateValue, segment: SegmentRegister)=
      new RegisterMemoryLocation(index, Some(displacement), segment) with QuadWordSize

  }

  implicit def indexWrapper(index: DestinationIndex with IndexRegister): RegisterMemoryLocation[DestinationIndex with IndexRegister] =
    new RegisterMemoryLocation[DestinationIndex with IndexRegister](index, None, index.defaultSegment)
}