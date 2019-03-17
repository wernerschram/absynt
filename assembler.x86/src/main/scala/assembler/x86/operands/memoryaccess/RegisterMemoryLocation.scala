package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.ProcessorMode
import assembler.x86.operands.ImmediateValue.ValueToByteImmediate
import assembler.x86.operands._
import assembler.x86.operations.AddressOperandInfo

import scala.language.implicitConversions

sealed class RegisterMemoryLocation(val reference: RegisterReference, displacement: Option[ImmediateValue with ByteWordDoubleSize], segment: SegmentRegister)
                                   (implicit byteImmediate: ValueToByteImmediate)
  extends IndirectMemoryLocation(reference.indexCode,
     if (reference.onlyWithDisplacement)
      Some(displacement.getOrElse(byteImmediate(0.toByte)))
    else
      displacement, segment)
    with ModRMEncodableOperand {
  self: ValueSize =>

  override val defaultSegment: SegmentRegister = reference.defaultSegment

  override val addressOperands: Set[AddressOperandInfo] = reference match {
    case bi: BaseIndexReference => Set(AddressOperandInfo.rmBase(bi.base), AddressOperandInfo.rmIndex(bi.index, segmentOverride))
    case o: GeneralPurposeRegister with IndexRegister with ValueSize => Set(AddressOperandInfo.rmIndex(o, segmentOverride))
  }

  override def toString: String = s"$sizeName PTR $segmentPrefix[$reference$displacementString]"

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
}

class DestinationReference(override val reference: RegisterReference with DestinationIndex, displacement: Option[ImmediateValue with ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate)
  extends RegisterMemoryLocation(reference, displacement, segment) {
  self: ValueSize =>
}

object RegisterMemoryLocation {
  abstract class RMForSize[Size<:ValueSize] {
    def instance(reference: RegisterReference, displacement: Option[ImmediateValue with ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate): RegisterMemoryLocation with Size
    def DestinationReference(reference: RegisterReference with DestinationIndex, displacement: Option[ImmediateValue with ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate): DestinationReference with Size
  }

  trait I8086Implicits {
    implicit def RMforByteSize: RMForSize[ByteSize] = new RMForSize[ByteSize] {
      override def instance(reference: RegisterReference, displacement: Option[ImmediateValue with ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate): RegisterMemoryLocation with ByteSize =
        new RegisterMemoryLocation(reference, displacement, segment) with ByteSize

      override def DestinationReference(reference: RegisterReference with DestinationIndex, displacement: Option[ImmediateValue with ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate): DestinationReference with ByteSize =
        new DestinationReference(reference, displacement, segment) with ByteSize
    }

    implicit def RMforWordSize: RMForSize[WordSize] = new RMForSize[WordSize] {
      override def instance(reference: RegisterReference, displacement: Option[ImmediateValue with ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate): RegisterMemoryLocation with WordSize =
        new RegisterMemoryLocation(reference, displacement, segment) with WordSize

      override def DestinationReference(reference: RegisterReference with DestinationIndex, displacement: Option[ImmediateValue with ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate): DestinationReference with WordSize =
        new DestinationReference(reference, displacement, segment) with WordSize
    }
  }


  trait I386Implicits {
    implicit def RMforDoubleWordSize: RMForSize[DoubleWordSize] = new RMForSize[DoubleWordSize] {
      override def instance(reference: RegisterReference, displacement: Option[ImmediateValue with ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate): RegisterMemoryLocation with DoubleWordSize =
        new RegisterMemoryLocation(reference, displacement, segment) with DoubleWordSize

      override def DestinationReference(reference: RegisterReference with DestinationIndex, displacement: Option[ImmediateValue with ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate): DestinationReference with DoubleWordSize =
        new DestinationReference(reference, displacement, segment) with DoubleWordSize
    }
  }

  trait X64Implicits {
    implicit def RMforQuadWordSize: RMForSize[QuadWordSize] = new RMForSize[QuadWordSize] {
      override def instance(reference: RegisterReference, displacement: Option[ImmediateValue with ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate): RegisterMemoryLocation with QuadWordSize =
        new RegisterMemoryLocation(reference, displacement, segment) with QuadWordSize

      override def DestinationReference(reference: RegisterReference with DestinationIndex, displacement: Option[ImmediateValue with ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate): DestinationReference with QuadWordSize =
        new DestinationReference(reference, displacement, segment) with QuadWordSize
    }
  }

  trait Operations {
    object RegisterMemoryLocation {
      def apply[Size<:ValueSize:RMForSize](index: RegisterReference)(implicit byteImmediate: ValueToByteImmediate): RegisterMemoryLocation with Size =
        implicitly[RMForSize[Size]].instance(index, None, index.defaultSegment)

      def apply[Size<:ValueSize:RMForSize](index: RegisterReference, displacement: ImmediateValue with ByteWordDoubleSize)(implicit byteImmediate: ValueToByteImmediate): RegisterMemoryLocation with Size =
        implicitly[RMForSize[Size]].instance(index, Some(displacement), index.defaultSegment)

      object withSegmentOverride {
        def apply[Size<:ValueSize:RMForSize](index: RegisterReference, segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate): RegisterMemoryLocation with Size =
          implicitly[RMForSize[Size]].instance(index, None, segment)

        def apply[Size<:ValueSize:RMForSize](index: RegisterReference, displacement: ImmediateValue with ByteWordDoubleSize, segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate): RegisterMemoryLocation with Size =
          implicitly[RMForSize[Size]].instance(index, Some(displacement), segment)
      }
    }
  }


}

object DestinationReference {
  import assembler.x86.operands.memoryaccess.RegisterMemoryLocation._

  implicit def apply[Size<:ValueSize:RMForSize](index: DestinationIndex with IndexRegister)(implicit byteImmediate: ValueToByteImmediate): DestinationReference with Size =
    implicitly[RMForSize[Size]].DestinationReference(index, None, index.defaultSegment)
}