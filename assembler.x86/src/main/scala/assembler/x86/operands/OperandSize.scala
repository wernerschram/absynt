package assembler.x86.operands

import assembler.x86.ProcessorMode

sealed abstract class OperandSize {
  def requiresOperandSizePrefix(processorMode: ProcessorMode): Boolean = false
}

sealed abstract class ValueSize(val size: Int, override val toString: String) extends OperandSize

object OperandSize {
  case object Unknown extends OperandSize

  case object Byte extends ValueSize(1, "BYTE")
  case object Word extends ValueSize(2, "WORD") {
    override def requiresOperandSizePrefix(processorMode: ProcessorMode) = processorMode match {
      case ProcessorMode.Protected | ProcessorMode.Long => true
      case default => false
    }
  }
  case object DoubleWord extends ValueSize(4, "DWORD") {
    override def requiresOperandSizePrefix(processorMode: ProcessorMode) = processorMode match {
      case ProcessorMode.Real => true
      case default => false
    }
  }
  case object QuadWord extends ValueSize(8, "QWORD")

  def apply(size: Int) = size match {
    case 1 => Byte
    case 2 => Word
    case 4 => DoubleWord
    case 8 => QuadWord
    case default => throw new AssertionError
  }
}