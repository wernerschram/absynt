package assembler.x86

import assembler.{AddressFactory, OffsetDirectionOld, OffsetFactory, RelativeOffset}
import assembler.x86.operands.memoryaccess._

sealed abstract class ProcessorMode

trait X86OffsetFactory[OffsetType<:X86Offset] extends OffsetFactory[OffsetType] {
}

object ProcessorMode {

  object Real extends ProcessorMode {
    implicit val offsetFactory: X86OffsetFactory[RealX86Offset] = new X86OffsetFactory[RealX86Offset] {
      override implicit def offset(offset: Long): RealX86Offset with RelativeOffset = RealRelativeOffset(offset)
      override def add(thisOffset: RealX86Offset, that: RealX86Offset with RelativeOffset): RealX86Offset with RelativeOffset =
        offset(thisOffset.offset + that.offset)
      override def add(thisOffset: RealX86Offset, that: Long): RealX86Offset with RelativeOffset =
        offset(thisOffset.offset + that)

      override implicit def positionalOffset(offsetValue: Long)(offsetDirection: OffsetDirectionOld)(instructionSize: Int): RealX86Offset with RelativeOffset =
        offsetDirection match {
          case OffsetDirectionOld.None => offsetFactory.offset(-instructionSize)
          case OffsetDirectionOld.Forward => offsetFactory.offset(offsetValue)
          case OffsetDirectionOld.Backward => offsetFactory.offset(-offsetValue - instructionSize)
        }
    }

    implicit val addressFactory: AddressFactory[RealX86Offset, FarPointer[RealX86Offset]] =
      new AddressFactory[RealX86Offset, FarPointer[RealX86Offset]] {
        override def zero = FarPointer(0.toShort, offsetFactory.offset(0))

        override def add(address: FarPointer[RealX86Offset], offset: RealX86Offset with RelativeOffset) =
          FarPointer(address.segment, offsetFactory.add(address.offset, offset))
      }

    implicit val processorMode: ProcessorMode = this
    implicit def offset(offset: Long): RealX86Offset with RelativeOffset = offsetFactory.offset(offset)
  }

  object Protected extends ProcessorMode {
    implicit val offsetFactory: X86OffsetFactory[ProtectedX86Offset] = new X86OffsetFactory[ProtectedX86Offset] {
      override implicit def offset(offset: Long): ProtectedX86Offset with RelativeOffset = ProtectedRelativeOffset(offset)

      override def add(thisOffset: ProtectedX86Offset, that: ProtectedX86Offset with RelativeOffset): ProtectedX86Offset with RelativeOffset =
        offset(thisOffset.offset + that.offset)
      override def add(thisOffset: ProtectedX86Offset, that: Long): ProtectedX86Offset with RelativeOffset =
      offset(thisOffset.offset + that)

      override implicit def positionalOffset(offsetValue: Long)(offsetDirection: OffsetDirectionOld)(instructionSize: Int): ProtectedX86Offset with RelativeOffset =
        offsetDirection match {
          case OffsetDirectionOld.None => offsetFactory.offset(-instructionSize)
          case OffsetDirectionOld.Forward => offsetFactory.offset(offsetValue)
          case OffsetDirectionOld.Backward => offsetFactory.offset(-offsetValue - instructionSize)
        }
    }

    implicit val addressFactory: AddressFactory[ProtectedX86Offset, FarPointer[ProtectedX86Offset]] =
      new AddressFactory[ProtectedX86Offset, FarPointer[ProtectedX86Offset]] {
        override def zero = FarPointer(0.toShort, offsetFactory.offset(0))

        override def add(address: FarPointer[ProtectedX86Offset], offset: ProtectedX86Offset with RelativeOffset) =
          FarPointer(address.segment, offsetFactory.add(address.offset, offset))
      }

    implicit val processorMode: ProcessorMode = this
    implicit def offset(offset: Long): ProtectedX86Offset with RelativeOffset = offsetFactory.offset(offset)
  }

  object Long extends ProcessorMode {
    implicit val offsetFactory: X86OffsetFactory[ProtectedX86Offset] = new X86OffsetFactory[ProtectedX86Offset] {
      override implicit def offset(offset: Long): ProtectedX86Offset with RelativeOffset = ProtectedRelativeOffset(offset)

      override def add(thisOffset: ProtectedX86Offset, that: ProtectedX86Offset with RelativeOffset): ProtectedX86Offset with RelativeOffset =
        offset(thisOffset.offset + that.offset)
      override def add(thisOffset: ProtectedX86Offset, that: Long): ProtectedX86Offset with RelativeOffset =
        offset(thisOffset.offset + that)

      override implicit def positionalOffset(offsetValue: Long)(offsetDirection: OffsetDirectionOld)(instructionSize: Int): ProtectedX86Offset with RelativeOffset =
        offsetDirection match {
        case OffsetDirectionOld.None => offsetFactory.offset(-instructionSize)
        case OffsetDirectionOld.Forward => offsetFactory.offset(offsetValue)
        case OffsetDirectionOld.Backward => offsetFactory.offset(-offsetValue - instructionSize)
      }
    }

    implicit val addressFactory: AddressFactory[ProtectedX86Offset, FarPointer[ProtectedX86Offset]] =
      new AddressFactory[ProtectedX86Offset, FarPointer[ProtectedX86Offset]] {
        override def zero = FarPointer(0.toShort, offsetFactory.offset(0))

        override def add(address: FarPointer[ProtectedX86Offset], offset: ProtectedX86Offset with RelativeOffset) =
          FarPointer(address.segment, offsetFactory.add(address.offset, offset))
      }

    implicit val processorMode: ProcessorMode = this
    implicit def offset(offset: Long): ProtectedX86Offset with RelativeOffset = offsetFactory.offset(offset)
  }
}
