package assembler.x86.operations

import assembler.x86.{ProcessorMode, X86OffsetFactory}
import assembler.x86.operands.memoryaccess.{LongPointer, ShortPointer, X86Offset, NearPointer => NearPointerOperand}
import assembler._

abstract class NearJumpOperation[OffsetType <: X86Offset: X86OffsetFactory](label: Label, shortOpcode: List[Byte], longOpcode: List[Byte], mnemonic: String, target: Label)
                                (implicit processorMode: ProcessorMode)
  extends ShortJumpOperation[OffsetType](label, shortOpcode, mnemonic, target) {

  val forwardShortLongBoundary: Byte = Byte.MaxValue
  val backwardShortLongBoundary: Int = (-Byte.MinValue) - shortJumpSize

  val longJumpSize: Int = longOpcode.length + (if (processorMode == ProcessorMode.Real) 2 else 4)

  override def estimateSize: Estimate[Int] = Estimate(shortJumpSize, longJumpSize)

  def encodableForLongPointer(pointer: NearPointerOperand[OffsetType]): Resource with Encodable

  override def sizeForDistance(offsetDirection: OffsetDirection, distance: Long): Int = offsetDirection match {
    case OffsetDirection.Backward if distance <= backwardShortLongBoundary => shortJumpSize
    case OffsetDirection.Forward if distance <= forwardShortLongBoundary => shortJumpSize
    case OffsetDirection.None => shortJumpSize
    case _ => longJumpSize
  }

  override def encodableForOffset(offset: OffsetType): Resource with Encodable = {
    if (offset.isShort(shortJumpSize))
      encodableForShortPointer(ShortPointer(offset))
    else
      encodableForLongPointer(LongPointer(offset))
  }
}
