package assembler.x86

import assembler.{AddressFactory, OffsetDirection, OffsetFactory, PositionalOffsetFactory}
import assembler.x86.operands.memoryaccess._

sealed abstract class ProcessorMode

trait X86OffsetFactory[OffsetType<:X86Offset] extends OffsetFactory[OffsetType] {
  def positionalOffsetFactory(): PositionalOffsetFactory[OffsetType]
}

object ProcessorMode {

  object Real extends ProcessorMode {
    implicit val offsetFactory: X86OffsetFactory[RealOffset] = new X86OffsetFactory[RealOffset] {
      override implicit def offset(offset: Long): RealOffset = RealOffset(offset)
      override def add(offset: RealOffset, that: RealOffset): RealOffset = offset + that
      override def add(offset: RealOffset, that: Long): RealOffset = offset + that

      def positionalOffsetFactory(): PositionalOffsetFactory[RealOffset] = new PositionalOffsetFactory[RealOffset] {
        override implicit def offset(instructionSize: Int, offsetDirection: OffsetDirection, offsetValue: Long): RealOffset = offsetDirection match {
          case OffsetDirection.None => offsetFactory.offset(-instructionSize)
          case OffsetDirection.Forward => offsetFactory.offset(offsetValue)
          case OffsetDirection.Backward => offsetFactory.offset(-offsetValue - instructionSize)
        }

        override def offset(offsetValue: Long): RealOffset = offsetFactory.offset(offsetValue)
        override def add(offset: RealOffset, that: RealOffset): RealOffset = offsetFactory.add(offset, that)
        override def add(offset: RealOffset, that: Long): RealOffset = offsetFactory.add(offset, that)
      }
    }

    implicit val addressFactory: AddressFactory[RealOffset, FarPointer[RealOffset]] = new AddressFactory[RealOffset, FarPointer[RealOffset]] {
      override def zero = FarPointer(0.toShort, offsetFactory.offset(0))
      override def add(address: FarPointer[RealOffset], offset: RealOffset) = FarPointer(address.segment, address.offset + offset)
    }

    implicit val processorMode: ProcessorMode = this
    implicit def offset(offset: Long): RealOffset = offsetFactory.offset(offset)
  }

  object Protected extends ProcessorMode {
    implicit val offsetFactory: X86OffsetFactory[ProtectedOffset] = new X86OffsetFactory[ProtectedOffset] {
      override implicit def offset(offset: Long): ProtectedOffset = ProtectedOffset(offset)

      override def add(offset: ProtectedOffset, that: ProtectedOffset): ProtectedOffset = offset.add(that)
      override def add(offset: ProtectedOffset, that: Long): ProtectedOffset = offset.add(that)

      def positionalOffsetFactory(): PositionalOffsetFactory[ProtectedOffset] = new PositionalOffsetFactory[ProtectedOffset] {
        override implicit def offset(instructionSize: Int, offsetDirection: OffsetDirection, offsetValue: Long): ProtectedOffset = offsetDirection match {
          case OffsetDirection.None => offsetFactory.offset(-instructionSize)
          case OffsetDirection.Forward => offsetFactory.offset(offsetValue)
          case OffsetDirection.Backward => offsetFactory.offset(-offsetValue - instructionSize)
        }

        override def offset(offsetValue: Long): ProtectedOffset = offsetFactory.offset(offsetValue)
        override def add(offset: ProtectedOffset, that: ProtectedOffset): ProtectedOffset = offsetFactory.add(offset, that)
        override def add(offset: ProtectedOffset, that: Long): ProtectedOffset = offsetFactory.add(offset, that)
      }
    }

    implicit val addressFactory: AddressFactory[ProtectedOffset, FarPointer[ProtectedOffset]] = new AddressFactory[ProtectedOffset, FarPointer[ProtectedOffset]] {
      override def zero = FarPointer(0.toShort, offsetFactory.offset(0))
      override def add(address: FarPointer[ProtectedOffset], offset: ProtectedOffset) = FarPointer(address.segment, address.offset + offset)
    }

    implicit val processorMode: ProcessorMode = this
    implicit def offset(offset: Long): ProtectedOffset = offsetFactory.offset(offset)
  }

  object Long extends ProcessorMode {
    implicit val offsetFactory: X86OffsetFactory[ProtectedOffset] = new X86OffsetFactory[ProtectedOffset] {
      override implicit def offset(offset: Long): ProtectedOffset = ProtectedOffset(offset)

      override def add(offset: ProtectedOffset, that: ProtectedOffset): ProtectedOffset = offset.add(that)
      override def add(offset: ProtectedOffset, that: Long): ProtectedOffset = offset.add(that)

      def positionalOffsetFactory(): PositionalOffsetFactory[ProtectedOffset] = new PositionalOffsetFactory[ProtectedOffset] {
        override implicit def offset(instructionSize: Int, offsetDirection: OffsetDirection, offsetValue: Long): ProtectedOffset = offsetDirection match {
          case OffsetDirection.None => offsetFactory.offset(-instructionSize)
          case OffsetDirection.Forward => offsetFactory.offset(offsetValue)
          case OffsetDirection.Backward => offsetFactory.offset(-offsetValue - instructionSize)
        }

        override def offset(offsetValue: Long): ProtectedOffset = offsetFactory.offset(offsetValue)
        override def add(offset: ProtectedOffset, that: ProtectedOffset): ProtectedOffset = offsetFactory.add(offset, that)
        override def add(offset: ProtectedOffset, that: Long): ProtectedOffset = offsetFactory.add(offset, that)
      }
    }

    implicit val addressFactory: AddressFactory[ProtectedOffset, FarPointer[ProtectedOffset]] = new AddressFactory[ProtectedOffset, FarPointer[ProtectedOffset]] {
      override def zero = FarPointer(0.toShort, offsetFactory.offset(0))
      override def add(address: FarPointer[ProtectedOffset], offset: ProtectedOffset) = FarPointer(address.segment, address.offset + offset)
    }

    implicit val processorMode: ProcessorMode = this
    implicit def offset(offset: Long): ProtectedOffset = offsetFactory.offset(offset)
  }
}
