package assembler.x86.operations

import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.{LongPointer, ShortPointer, X86Offset, NearPointer => NearPointerOperand}
import assembler.{Encodable, Label, Resource}

abstract class NearJumpOperation(label: Label, shortOpcode: List[Byte], longOpcode: List[Byte], mnemonic: String, target: Label)
                                (implicit processorMode: ProcessorMode)
  extends ShortJumpOperation(label, shortOpcode, mnemonic, target) {

  val forwardShortLongBoundary: Byte = Byte.MaxValue
  val backwardShortLongBoundary: Int = (-Byte.MinValue) - shortJumpSize

  val longJumpSize: Int = processorMode match {
    case ProcessorMode.Real => longOpcode.length + 2
    case ProcessorMode.Protected | ProcessorMode.Long => longOpcode.length + 4
  }

  override val maximumSize: Int = longJumpSize

  def encodableForRealLongPointer(pointer: NearPointerOperand[X86Offset.RealLongOffset]): Resource with Encodable
  def encodableForProtectedLongPointer(pointer: NearPointerOperand[X86Offset.ProtectedLongOffset]): Resource with Encodable

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
        encodableForShortPointer(ShortPointer(distance.asInstanceOf[X86Offset.ShortOffset]))
      } else {
        if (processorMode == ProcessorMode.Real) {
          encodableForRealLongPointer(LongPointer(distance.asInstanceOf[X86Offset.RealLongOffset]))
        } else {
          encodableForProtectedLongPointer(LongPointer(distance.asInstanceOf[X86Offset.ProtectedLongOffset]))
        }
      }
    } else {
      if (distance <= backwardShortLongBoundary) {
        encodableForShortPointer(ShortPointer((-distance - shortJumpSize).asInstanceOf[X86Offset.ShortOffset]))
      } else {
        if (processorMode == ProcessorMode.Real) {
          encodableForRealLongPointer(LongPointer((-distance - longJumpSize).asInstanceOf[X86Offset.RealLongOffset]))
        } else {
          encodableForProtectedLongPointer(LongPointer((-distance - longJumpSize).asInstanceOf[X86Offset.ProtectedLongOffset]))
        }
      }
    }
  }
}
