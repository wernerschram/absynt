package assembler.arm

import assembler._
import assembler.arm.operands.{ArmOffset, ArmRelativeOffset}

sealed abstract class ProcessorMode {
  implicit val processorMode: ProcessorMode = this
}

trait ArmOffsetFactory extends OffsetFactory[ArmOffset] {
}

object ProcessorMode {
  case object A32 extends ProcessorMode {
    implicit val offsetFactory: ArmOffsetFactory = new ArmOffsetFactory {
      override implicit def offset(offset: Long): ArmRelativeOffset = ArmRelativeOffset(offset.toInt)
    }
  }

  case object Thumb extends ProcessorMode {
    implicit val offsetFactory: ArmOffsetFactory = new ArmOffsetFactory {
      override implicit def offset(offset: Long): ArmRelativeOffset = ArmRelativeOffset(offset.toInt)
    }
  }
}
