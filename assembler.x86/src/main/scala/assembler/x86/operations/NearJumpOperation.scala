package assembler.x86.operations

import assembler._
import assembler.resource.{UnlabeledEncodable, Resource}
import assembler.x86.operands.memoryaccess.{LongPointer, ShortPointer, X86Offset, NearPointer => NearPointerOperand}
import assembler.x86.{ProcessorMode, X86OffsetFactory}

abstract class NearJumpOperation[OffsetType <: X86Offset: X86OffsetFactory](shortOpcode: Seq[Byte], longOpcode: Seq[Byte], mnemonic: String, target: Label)
                                (implicit processorMode: ProcessorMode)
  extends ShortJumpOperation[OffsetType](shortOpcode, mnemonic, target) {

  val forwardShortLongBoundary: Byte = Byte.MaxValue
  val backwardShortLongBoundary: Int = (-Byte.MinValue) - shortJumpSize

  val longJumpSize: Int = longOpcode.length + (if (processorMode == ProcessorMode.Real) 2 else 4)

  override def possibleSizes: Set[Int] = Set(shortJumpSize, longJumpSize)

  def encodableForLongPointer(pointer: NearPointerOperand[OffsetType]): Resource with UnlabeledEncodable

  override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Resource with UnlabeledEncodable = {
    val offset = offsetFactory.positionalOffset(distance)(offsetDirection)(shortJumpSize)
    if (offset.isShort(shortJumpSize))
      encodableForShortPointer(ShortPointer(offset))
    else
      encodableForLongPointer(LongPointer(offsetFactory.positionalOffset(distance)(offsetDirection)(longJumpSize)))
  }
}
