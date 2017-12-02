package assembler.x86

import assembler._
import assembler.x86.operands.memoryaccess._

sealed abstract class ProcessorMode

trait X86OffsetFactory[OffsetType<:X86Offset] extends OffsetFactory[OffsetType] {
}

object ProcessorMode {

  object Real extends ProcessorMode {
    implicit val offsetFactory: X86OffsetFactory[RealX86Offset] = new X86OffsetFactory[RealX86Offset] {
      override implicit def offset(offset: Long): RealX86Offset with RelativeOffset = RealRelativeOffset(offset)

      override implicit def positionalOffset(offsetValue: Long)(offsetDirection: RelativeOffsetDirection)(instructionSize: Int): RealX86Offset with RelativeOffset =
        offsetDirection match {
          case OffsetDirection.Self => RealRelativeOffset(-instructionSize)
          case OffsetDirection.Forward => RealRelativeOffset(offsetValue)
          case OffsetDirection.Backward => RealRelativeOffset(-offsetValue - instructionSize)
        }
    }

    implicit val processorMode: ProcessorMode = this
    implicit def offset(offset: Long): RealX86Offset with RelativeOffset = offsetFactory.offset(offset)
  }

  object Protected extends ProcessorMode {
    implicit val offsetFactory: X86OffsetFactory[ProtectedX86Offset] = new X86OffsetFactory[ProtectedX86Offset] {
      override implicit def offset(offset: Long): ProtectedX86Offset with RelativeOffset = ProtectedRelativeOffset(offset)

      override implicit def positionalOffset(offsetValue: Long)(offsetDirection: RelativeOffsetDirection)(instructionSize: Int): ProtectedX86Offset with RelativeOffset =
        offsetDirection match {
          case OffsetDirection.Self => ProtectedRelativeOffset(-instructionSize)
          case OffsetDirection.Forward => ProtectedRelativeOffset(offsetValue)
          case OffsetDirection.Backward => ProtectedRelativeOffset(-offsetValue - instructionSize)
        }
    }

    implicit val processorMode: ProcessorMode = this
    implicit def offset(offset: Long): ProtectedX86Offset with RelativeOffset = offsetFactory.offset(offset)
  }

  object Long extends ProcessorMode {
    implicit val offsetFactory: X86OffsetFactory[ProtectedX86Offset] = new X86OffsetFactory[ProtectedX86Offset] {
      override implicit def offset(offset: Long): ProtectedX86Offset with RelativeOffset = ProtectedRelativeOffset(offset)

      override implicit def positionalOffset(offsetValue: Long)(offsetDirection: RelativeOffsetDirection)(instructionSize: Int): ProtectedX86Offset with RelativeOffset =
        offsetDirection match {
        case OffsetDirection.Self => ProtectedRelativeOffset(-instructionSize)
        case OffsetDirection.Forward => ProtectedRelativeOffset(offsetValue)
        case OffsetDirection.Backward => ProtectedRelativeOffset(-offsetValue - instructionSize)
      }
    }

    implicit val processorMode: ProcessorMode = this
    implicit def offset(offset: Long): ProtectedX86Offset with RelativeOffset = offsetFactory.offset(offset)
  }
}
