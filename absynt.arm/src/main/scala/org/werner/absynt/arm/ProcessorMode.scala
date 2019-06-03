package org.werner.absynt.arm

import org.werner.absynt._
import org.werner.absynt.arm.operands.{ArmOffset, ArmRelativeOffset}

sealed abstract class ProcessorMode {
  implicit val processorMode: ProcessorMode = this
}

object ProcessorMode {
  case object A32 extends ProcessorMode {
  }

  case object Thumb extends ProcessorMode {
  }
}
