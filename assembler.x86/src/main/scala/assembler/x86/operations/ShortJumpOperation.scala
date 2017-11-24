package assembler.x86.operations

import assembler.reference.SinglePassRelativeReference
import assembler.x86.operands.memoryaccess.{ShortPointer, X86Offset, X86RelativeOffset, NearPointer => NearPointerOperand}
import assembler._
import assembler.x86.X86OffsetFactory

abstract class ShortJumpOperation[OffsetType <: X86Offset]
  (val label: Label, val shortOpcode: List[Byte], mnemonic: String, override val target: Label)
  (implicit val offsetFactory: X86OffsetFactory[OffsetType])
    extends SinglePassRelativeReference[OffsetType] {

  val shortJumpSize: Int = shortOpcode.length + 1

  override def estimateSize: Estimate[Int] = Actual(shortJumpSize)

  def encodableForShortPointer(pointer: NearPointerOperand[OffsetType]): Resource with Encodable

  override def toString = s"$labelPrefix$mnemonic $target"

  override def sizeForDistance(offsetDirection: OffsetDirectionOld, distance: Long): Int = shortJumpSize

  override def encodeForDistance(distance: Int): Resource with Encodable = encodableForShortPointer(ShortPointer[OffsetType](offsetFactory.offset(distance)))

  override def sizeForDistance(distance: Int): Int = encodeForDistance(distance).size

  override def possibleSizes: List[Int] = shortOpcode.length + 1 :: Nil

  override def encodableForOffset(offset: OffsetType with RelativeOffset): Resource with Encodable = {
    assume(offset.isShort(shortJumpSize))
    encodableForShortPointer(ShortPointer(offset))
  }
}
