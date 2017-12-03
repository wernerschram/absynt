package assembler.arm

import assembler._
import assembler.arm.operands.{ArmOffset, ArmRelativeOffset}

sealed abstract class ProcessorMode {
  implicit val processorMode: ProcessorMode = this
}

object ProcessorMode {
  case object A32 extends ProcessorMode {
  }

  case object Thumb extends ProcessorMode {
  }
}
