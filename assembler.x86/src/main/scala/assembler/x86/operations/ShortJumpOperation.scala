package assembler.x86.operations

import assembler._
import assembler.resource.{Encodable, RelativeReference, Resource}
import assembler.x86.X86OffsetFactory
import assembler.x86.operands.memoryaccess.{ShortPointer, X86Offset, NearPointer => NearPointerOperand}

abstract class ShortJumpOperation[OffsetType <: X86Offset]
  (val label: Label, val shortOpcode: List[Byte], mnemonic: String, override val target: Label)
  (implicit val offsetFactory: X86OffsetFactory[OffsetType])
    extends RelativeReference {

  val shortJumpSize: Int = shortOpcode.length + 1

  def encodableForShortPointer(pointer: NearPointerOperand[OffsetType]): Resource with Encodable

  override def toString = s"$labelPrefix$mnemonic $target"

  override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Resource with Encodable =
    encodableForShortPointer(ShortPointer[OffsetType](offsetFactory.positionalOffset(distance)(offsetDirection)(shortJumpSize)))

  override def sizeForDependencySize(distance: Int, offsetDirection: OffsetDirection): Int =
    encodableForDependencySize(distance, offsetDirection).size

  override def possibleSizes: Set[Int] = Set(shortJumpSize)
}
