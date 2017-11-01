package assembler.arm

import assembler.{AddressFactory, OffsetDirection, OffsetFactory, PositionalOffsetFactory}
import assembler.arm.operands.{ArmOffset, RelativeA32Pointer, RelativeThumbPointer}

sealed abstract class ProcessorMode {
  implicit val processorMode: ProcessorMode = this
}

trait ArmOffsetFactory extends OffsetFactory[ArmOffset] {
  def positionalOffsetFactory: PositionalOffsetFactory[ArmOffset]
}

object ProcessorMode {
  case object A32 extends ProcessorMode {
    implicit val offsetFactory: ArmOffsetFactory = new ArmOffsetFactory {
      override implicit def offset(offset: Long): ArmOffset = ArmOffset(offset.toInt)

      override def add(offset: ArmOffset, that: ArmOffset): ArmOffset = offset + that

      override def add(offset: ArmOffset, that: Long): ArmOffset = offset + that


      override def positionalOffsetFactory: PositionalOffsetFactory[ArmOffset] =
        new PositionalOffsetFactory[ArmOffset] {
          override implicit def offset(instructionSize: Int, offsetDirection: OffsetDirection, offsetValue: Long): ArmOffset = offsetDirection match {
            case OffsetDirection.None => offsetFactory.offset(-instructionSize - 4)
            case OffsetDirection.Forward => offsetFactory.offset(offsetValue - 4)
            case OffsetDirection.Backward => offsetFactory.offset(-offsetValue - (instructionSize + 4))
          }
          // FIXME: add.forRelativeLabel(pc, x, label) should result in the sum of the address of label and the address
          // FIXME: of the first operation in the set produced by add.forConstant for the given target plus 8

          override def offset(offsetValue: Long): ArmOffset = offsetFactory.offset(offsetValue)

          override def add(offset: ArmOffset, that: ArmOffset): ArmOffset = offsetFactory.add(offset, that)

          override def add(offset: ArmOffset, that: Long): ArmOffset = offsetFactory.add(offset, that)
        }
    }

    implicit val addressFactory: AddressFactory[ArmOffset, RelativeA32Pointer] = new AddressFactory[ArmOffset, RelativeA32Pointer] {
      override def zero = RelativeA32Pointer(offsetFactory.offset(0))

      override def add(address: RelativeA32Pointer, offset: ArmOffset) = RelativeA32Pointer(address.offset + offset)
    }

  }

  case object Thumb extends ProcessorMode {
    implicit val offsetFactory: ArmOffsetFactory = new ArmOffsetFactory {
      override implicit def offset(offset: Long): ArmOffset = ArmOffset(offset.toInt)

      override def add(offset: ArmOffset, that: ArmOffset): ArmOffset = offset + that

      override def add(offset: ArmOffset, that: Long): ArmOffset = offset + that

      override def positionalOffsetFactory: PositionalOffsetFactory[ArmOffset] =
        new PositionalOffsetFactory[ArmOffset] {
          override implicit def offset(instructionSize: Int, offsetDirection: OffsetDirection, offsetValue: Long): ArmOffset = offsetDirection match {
            case OffsetDirection.None => offsetFactory.offset(-instructionSize - 8)
            case OffsetDirection.Forward => offsetFactory.offset(offsetValue - 8)
            case OffsetDirection.Backward => offsetFactory.offset(-offsetValue - instructionSize - 8)
          }

          override def offset(offsetValue: Long): ArmOffset = offsetFactory.offset(offsetValue)

          override def add(offset: ArmOffset, that: ArmOffset): ArmOffset = offsetFactory.add(offset, that)

          override def add(offset: ArmOffset, that: Long): ArmOffset = offsetFactory.add(offset, that)
        }

    }

    implicit val addressFactory: AddressFactory[ArmOffset, RelativeThumbPointer] = new AddressFactory[ArmOffset, RelativeThumbPointer] {
      override def zero = RelativeThumbPointer(offsetFactory.offset(0))

      override def add(address: RelativeThumbPointer, offset: ArmOffset) = RelativeThumbPointer(address.offset + offset)
    }

  }
}
