package assembler.x86.operations

import assembler.x86.operands.memoryaccess.{LongPointer, X86Offset, NearPointer => NearPointerOperand, ShortPointer}
import assembler.x86.{ProcessorMode, ProcessorModeWithOffset}
import assembler.{Encodable, Label, Resource}

abstract class NearJumpOperation[OffsetType <: X86Offset](label: Label, shortOpcode: List[Byte], longOpcode: List[Byte], mnemonic: String, target: Label)
                                (implicit processorMode: ProcessorModeWithOffset[OffsetType])
  extends ShortJumpOperation[OffsetType](label, shortOpcode, mnemonic, target) {

  val forwardShortLongBoundary: Byte = Byte.MaxValue
  val backwardShortLongBoundary: Int = (-Byte.MinValue) - shortJumpSize

  val longJumpSize: Int = if (processorMode == ProcessorMode.Real) {
    longOpcode.length + 2
  } else {
    longOpcode.length + 4
  }

  override val maximumSize: Int = longJumpSize

  def encodableForLongPointer(pointer: NearPointerOperand[OffsetType]): Resource with Encodable

  override def sizeForDistance(distance: Int)(forward: Boolean): Int =
    if (forward) {
      if (distance <= forwardShortLongBoundary)
        shortJumpSize
      else
        longJumpSize
    } else {
      if (distance <= backwardShortLongBoundary)
        shortJumpSize
      else
        longJumpSize
    }

  override def encodableForDistance(distance: Int)(forward: Boolean): Resource with Encodable = {
    if (forward) {
      if (distance <= forwardShortLongBoundary) {
        encodableForShortPointer(ShortPointer(implicitly[ProcessorModeWithOffset[OffsetType]].offset(distance)))
      } else {
        encodableForLongPointer(LongPointer(implicitly[ProcessorModeWithOffset[OffsetType]].offset(distance)))
      }
    } else {
      if (distance <= backwardShortLongBoundary) {
        encodableForShortPointer(ShortPointer(implicitly[ProcessorModeWithOffset[OffsetType]].offset(-distance - shortJumpSize)))
      } else {
        encodableForLongPointer(LongPointer(implicitly[ProcessorModeWithOffset[OffsetType]].offset(-distance - longJumpSize)))
      }
    }
  }
}
