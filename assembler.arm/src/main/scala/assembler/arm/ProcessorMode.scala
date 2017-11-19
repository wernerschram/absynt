package assembler.arm

import assembler.{AddressFactory, OffsetDirection, OffsetFactory, RelativeOffset}
import assembler.arm.operands.{ArmOffset, ArmRelativeOffset, RelativeA32Pointer, RelativeThumbPointer}

sealed abstract class ProcessorMode {
  implicit val processorMode: ProcessorMode = this
}

trait ArmOffsetFactory extends OffsetFactory[ArmOffset] {
}

object ProcessorMode {
  case object A32 extends ProcessorMode {
    implicit val offsetFactory: ArmOffsetFactory = new ArmOffsetFactory {
      override implicit def offset(offset: Long): ArmRelativeOffset = ArmRelativeOffset(offset.toInt)

      override def add(thisOffset: ArmOffset, that: ArmOffset with RelativeOffset): ArmOffset with RelativeOffset = thisOffset + that

      override def add(thisOffset: ArmOffset, that: Long): ArmOffset with RelativeOffset = thisOffset + that

      override implicit def positionalOffset(offsetValue: Long)(offsetDirection: OffsetDirection)(instructionSize: Int): ArmOffset with RelativeOffset =
        offsetDirection match {
          case OffsetDirection.None => offsetFactory.offset(-instructionSize - 4)
          case OffsetDirection.Forward => offsetFactory.offset(offsetValue - 4)
          case OffsetDirection.Backward => offsetFactory.offset(-offsetValue - (instructionSize + 4))
        }

    }

    implicit val addressFactory: AddressFactory[ArmOffset, RelativeA32Pointer] = new AddressFactory[ArmOffset, RelativeA32Pointer] {
      override def zero = RelativeA32Pointer(offsetFactory.offset(0))

      override def add(address: RelativeA32Pointer, offset: ArmOffset with RelativeOffset) = RelativeA32Pointer(address.offset + offset)
    }

  }

  case object Thumb extends ProcessorMode {
    implicit val offsetFactory: ArmOffsetFactory = new ArmOffsetFactory {
      override implicit def offset(offset: Long): ArmRelativeOffset = ArmRelativeOffset(offset.toInt)

      override def add(thisOffset: ArmOffset, that: ArmOffset with RelativeOffset): ArmOffset with RelativeOffset = thisOffset + that

      override def add(thisOffset: ArmOffset, that: Long): ArmOffset with RelativeOffset = thisOffset + that

      override implicit def positionalOffset(offsetValue: Long)(offsetDirection: OffsetDirection)(instructionSize: Int): ArmOffset with RelativeOffset =
        offsetDirection match {
          case OffsetDirection.None => offsetFactory.offset(-instructionSize - 8)
          case OffsetDirection.Forward => offsetFactory.offset(offsetValue - 8)
          case OffsetDirection.Backward => offsetFactory.offset(-offsetValue - instructionSize - 8)
        }
    }

    implicit val addressFactory: AddressFactory[ArmOffset, RelativeThumbPointer] = new AddressFactory[ArmOffset, RelativeThumbPointer] {
      override def zero = RelativeThumbPointer(offsetFactory.offset(0))

      override def add(address: RelativeThumbPointer, offset: ArmOffset with RelativeOffset) = RelativeThumbPointer(address.offset + offset)
    }

  }
}
