package assembler.x86.operands

import assembler.x86.ProcessorMode

sealed class OperandSize {
  def requiresOperandSizePrefix(processorMode: ProcessorMode): Boolean = false // linter:ignore UnusedParameter
  def requiresAddressSizePrefix(processorMode: ProcessorMode): Boolean = false // linter:ignore UnusedParameter
}

sealed class ValueSize(override val toString: String) extends OperandSize

sealed class FarPointerSize(override val toString: String) extends OperandSize

object OperandSize {
  case object Unknown extends OperandSize
}

object ValueSize {
  case object Byte extends ValueSize("BYTE")

  case object Word extends ValueSize("WORD") {
    override def requiresOperandSizePrefix(processorMode: ProcessorMode) = processorMode match {
      case ProcessorMode.Protected | ProcessorMode.Long => true
      case default => false
    }

    override def requiresAddressSizePrefix(processorMode: ProcessorMode) = processorMode match {
      case ProcessorMode.Protected => true
      case default => false
    }
  }

  case object DoubleWord extends ValueSize("DWORD") {
    override def requiresOperandSizePrefix(processorMode: ProcessorMode) = processorMode match {
      case ProcessorMode.Real => true
      case default => false
    }

    override def requiresAddressSizePrefix(processorMode: ProcessorMode) = processorMode match {
      case ProcessorMode.Real | ProcessorMode.Long => true
      case default => false
    }
  }

  case object QuadWord extends ValueSize("QWORD")

  def sizeOfValue(size: Int) = size match {
    case 1 => Byte
    case 2 => Word
    case 4 => DoubleWord
    case 8 => QuadWord
    case default => throw new AssertionError
  }
}

object FarPointerSize {
  case object DoubleWord extends FarPointerSize("DWORD") {
    override def requiresOperandSizePrefix(processorMode: ProcessorMode) = processorMode match {
      case ProcessorMode.Real => false
      case default => true
    }
  }

  case object FarWord extends FarPointerSize("FWORD") {
    override def requiresOperandSizePrefix(processorMode: ProcessorMode) = processorMode match {
      case ProcessorMode.Real => true
      case default => false
    }
  }

  def sizeOfFarPointer(segmentSize: Int, offsetSize: Int) = (segmentSize, offsetSize) match {
    case (2, 2) => DoubleWord
    case (2, 4) => FarWord
    case default => throw new AssertionError
  }
}
