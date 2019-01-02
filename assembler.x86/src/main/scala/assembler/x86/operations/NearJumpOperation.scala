package assembler.x86.operations

import assembler._
import assembler.resource.{Resource, UnlabeledEncodable}
import assembler.x86.ProcessorMode
import assembler.x86.operands.WideSize
import assembler.x86.operands.memoryaccess.{LongPointer, ShortPointer, NearPointer => NearPointerOperand}

abstract class NearJumpOperation(shortOpcode: Seq[Byte], longOpcode: Seq[Byte], mnemonic: String, target: Label)
                                (implicit processorMode: ProcessorMode)
  extends ShortJumpOperation(shortOpcode, mnemonic, target) {

  val forwardShortLongBoundary: Byte = Byte.MaxValue
  val backwardShortLongBoundary: Int = (-Byte.MinValue) - shortJumpSize

  val longJumpSize: Int = longOpcode.length + (if (processorMode == ProcessorMode.Real) 2 else 4)

  override def possibleSizes: Set[Int] = Set(shortJumpSize, longJumpSize)

  def encodableForLongPointer[Size<:WideSize](pointer: NearPointerOperand with Size): Resource with UnlabeledEncodable

  override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Resource with UnlabeledEncodable = {
    val shortOffset = offsetDirection match {
      case OffsetDirection.Self => -shortJumpSize
      case OffsetDirection.Forward => distance
      case OffsetDirection.Backward => -distance - shortJumpSize
    }
    if (shortOffset.toByte == shortOffset)
      encodableForShortPointer(ShortPointer(shortOffset.toByte))
    else {
      val longOffset = offsetDirection match {
        case OffsetDirection.Self => -longJumpSize
        case OffsetDirection.Forward => distance
        case OffsetDirection.Backward => -distance - longJumpSize
      }

      processorMode match {
        case ProcessorMode.Real =>
          encodableForLongPointer(LongPointer.realMode(longOffset))
        case _ =>
          encodableForLongPointer(LongPointer.protectedMode(longOffset))
      }
    }
  }
}
