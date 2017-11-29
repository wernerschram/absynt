package assembler.arm

import assembler._
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

      override implicit def positionalOffset(offsetValue: Long)(offsetDirection: RelativeOffsetDirection)(instructionSize: Int): ArmOffset with RelativeOffset =
        offsetDirection match {
          case OffsetDirection.Self => offsetFactory.offset(-instructionSize - 4)
          case OffsetDirection.Forward => offsetFactory.offset(offsetValue - 4)
          case OffsetDirection.Backward => offsetFactory.offset(-offsetValue - (instructionSize + 4))
        }
    }
  }

  case object Thumb extends ProcessorMode {
    implicit val offsetFactory: ArmOffsetFactory = new ArmOffsetFactory {
      override implicit def offset(offset: Long): ArmRelativeOffset = ArmRelativeOffset(offset.toInt)

      override def add(thisOffset: ArmOffset, that: ArmOffset with RelativeOffset): ArmOffset with RelativeOffset = thisOffset + that

      override def add(thisOffset: ArmOffset, that: Long): ArmOffset with RelativeOffset = thisOffset + that

      override implicit def positionalOffset(offsetValue: Long)(offsetDirection: RelativeOffsetDirection)(instructionSize: Int): ArmOffset with RelativeOffset =
        offsetDirection match {
          case OffsetDirection.Self => offsetFactory.offset(-instructionSize - 8)
          case OffsetDirection.Forward => offsetFactory.offset(offsetValue - 8)
          case OffsetDirection.Backward => offsetFactory.offset(-offsetValue - instructionSize - 8)
        }
    }
  }
}
