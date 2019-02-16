package assembler.x86

import assembler.x86.operands.ImmediateValue

sealed abstract class ProcessorMode extends ImmediateValue.ForMode {
  self: ImmediateValue.ForMode =>
}

object ProcessorMode {

  object Legacy extends ProcessorMode with ImmediateValue.ForLegacy {
    implicit val processorMode: ProcessorMode = this
  }

  object Real extends ProcessorMode with ImmediateValue.ForReal {
    implicit val processorMode: ProcessorMode = this
  }

  object Protected extends ProcessorMode with ImmediateValue.ForProtected {

    implicit val processorMode: ProcessorMode = this
  }

  object Long extends ProcessorMode with ImmediateValue.ForLong {

    implicit val processorMode: ProcessorMode = this
  }
}
