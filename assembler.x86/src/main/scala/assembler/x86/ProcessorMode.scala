package assembler.x86

import assembler._
import assembler.x86.operands.memoryaccess._

sealed abstract class ProcessorMode

object ProcessorMode {

  object Real extends ProcessorMode {

    implicit val processorMode: ProcessorMode = this
  }

  object Protected extends ProcessorMode {

    implicit val processorMode: ProcessorMode = this
  }

  object Long extends ProcessorMode {

    implicit val processorMode: ProcessorMode = this
  }
}
