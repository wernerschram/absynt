package assembler.x86

sealed abstract class ProcessorMode {
}

object ProcessorMode {

  case object Real extends ProcessorMode
  case object Protected extends ProcessorMode
  case object Long extends ProcessorMode

}
