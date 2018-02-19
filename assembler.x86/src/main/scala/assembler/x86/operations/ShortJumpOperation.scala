package assembler.x86.operations

import assembler._
import assembler.resource.{RelativeReference, Resource, UnlabeledEncodable}
import assembler.x86.operands.memoryaccess.{ShortPointer, NearPointer => NearPointerOperand}

abstract class ShortJumpOperation
  (val shortOpcode: Seq[Byte], mnemonic: String, target: Label)
    extends RelativeReference(target) {

  val shortJumpSize: Int = shortOpcode.length + 1

  def encodableForShortPointer(pointer: NearPointerOperand): Resource with UnlabeledEncodable

  override def toString = s"$mnemonic $target"

  override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Resource with UnlabeledEncodable =
  {
    val offset = offsetDirection match {
      case OffsetDirection.Self => -shortJumpSize
      case OffsetDirection.Forward => distance
      case OffsetDirection.Backward => -distance - shortJumpSize
    }
    encodableForShortPointer(ShortPointer(offset))
  }

  override def sizeForDependencySize(distance: Int, offsetDirection: OffsetDirection): Int =
    encodableForDependencySize(distance, offsetDirection).size

  override def possibleSizes: Set[Int] = Set(shortJumpSize)
}
