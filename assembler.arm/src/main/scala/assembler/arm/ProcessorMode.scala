package assembler.arm

sealed abstract class ProcessorMode {
}

object ProcessorMode {
  case object A32 extends ProcessorMode
}
