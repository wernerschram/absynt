package org.werner.absynt.x86.operations

import org.werner.absynt._
import org.werner.absynt.resource.{Resource, UnlabeledEncodable}
import org.werner.absynt.x86.operands.WordDoubleQuadSize

abstract class NearJumpOperation[Size<:WordDoubleQuadSize](shortOpcode: Seq[Byte], longOpcode: Seq[Byte], mnemonic: String, target: Label, longJumpSize: Int)
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
