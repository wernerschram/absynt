package assembler.x86.operations

import assembler.reference.RelativeReference
import assembler.x86.operands.memoryaccess.{ShortPointer, X86Offset, NearPointer => NearPointerOperand}
import assembler._
import assembler.x86.X86OffsetFactory

abstract class ShortJumpOperation[OffsetType <: X86Offset: X86OffsetFactory](val label: Label, val shortOpcode: List[Byte], mnemonic: String, override val target: Label)
  extends Resource with RelativeReference[OffsetType] {

  val shortJumpSize: Int = shortOpcode.length + 1

  override val minimumSize: Int = shortJumpSize
  override val maximumSize: Int = shortJumpSize

  def encodableForShortPointer(pointer: NearPointerOperand[OffsetType]): Resource with Encodable

  override def toString = s"$labelPrefix$mnemonic $target"

  override def sizeForDistance(offsetDirection: OffsetDirection, distance: Long): Int = shortJumpSize

  override def encodableForOffset(offset: OffsetType): Resource with Encodable = {
    assume(offset.isShort(shortJumpSize))
    encodableForShortPointer(ShortPointer(offset))
  }

  override implicit def offsetFactory: PositionalOffsetFactory[OffsetType] = implicitly[X86OffsetFactory[OffsetType]].positionalOffsetFactory()
}
