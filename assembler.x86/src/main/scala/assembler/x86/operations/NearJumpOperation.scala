package assembler.x86.operations

import assembler._
import assembler.resource.{Resource, UnlabeledEncodable}
import assembler.x86.operands.WideSize

abstract class NearJumpOperation[Size<:WideSize](shortOpcode: Seq[Byte], longOpcode: Seq[Byte], mnemonic: String, target: Label, longJumpSize: Int)
  extends ShortJumpOperation(shortOpcode, mnemonic, target) {

  val forwardShortLongBoundary: Byte = Byte.MaxValue
  val backwardShortLongBoundary: Int = (-Byte.MinValue) - shortJumpSize

  override def possibleSizes: Set[Int] = Set(shortJumpSize, longJumpSize)

  def encodableForLongPointer(offset: Int): Resource with UnlabeledEncodable

  override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Resource with UnlabeledEncodable = {
    val shortOffset = offsetDirection match {
      case OffsetDirection.Self => -shortJumpSize
      case OffsetDirection.Forward => distance
      case OffsetDirection.Backward => -distance - shortJumpSize
    }
    if (shortOffset.toByte == shortOffset)
      encodableForShortPointer(shortOffset.toByte)
    else {
      val longOffset = offsetDirection match {
        case OffsetDirection.Self => -longJumpSize
        case OffsetDirection.Forward => distance
        case OffsetDirection.Backward => -distance - longJumpSize
      }

      encodableForLongPointer(longOffset)
    }
  }
}
