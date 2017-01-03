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
    override def requiresOperandSizePrefix(processorMode: ProcessorMode): Boolean = processorMode match {
      case ProcessorMode.Protected | ProcessorMode.Long => true
      case _ => false
    }

    override def requiresAddressSizePrefix(processorMode: ProcessorMode): Boolean = processorMode match {
      case ProcessorMode.Protected => true
      case _ => false
    }
  }

  case object DoubleWord extends ValueSize("DWORD") {
    override def requiresOperandSizePrefix(processorMode: ProcessorMode): Boolean = processorMode match {
      case ProcessorMode.Real => true
      case _ => false
    }

    override def requiresAddressSizePrefix(processorMode: ProcessorMode): Boolean = processorMode match {
      case ProcessorMode.Real | ProcessorMode.Long => true
      case _ => false
    }
  }

  case object QuadWord extends ValueSize("QWORD")

  def sizeOfValue(size: Int): ValueSize = size match {
    case 1 => Byte
    case 2 => Word
    case 4 => DoubleWord
    case 8 => QuadWord
    case _ => throw new AssertionError
  }
}

object FarPointerSize {
  case object DoubleWord extends FarPointerSize("DWORD") {
    override def requiresOperandSizePrefix(processorMode: ProcessorMode): Boolean = processorMode match {
      case ProcessorMode.Real => false
      case _ => true
    }
  }

  case object FarWord extends FarPointerSize("FWORD") {
    override def requiresOperandSizePrefix(processorMode: ProcessorMode): Boolean = processorMode match {
      case ProcessorMode.Real => true
      case _ => false
    }
  }

  def sizeOfFarPointer(segmentSize: Int, offsetSize: Int): FarPointerSize = (segmentSize, offsetSize) match {
    case (2, 2) => DoubleWord
    case (2, 4) => FarWord
    case _ => throw new AssertionError
  }
}
