package assembler.x86.operations

import assembler.reference.RelativeReference
import assembler.x86.ProcessorModeWithOffset
import assembler.x86.operands.memoryaccess.{ShortPointer, X86Offset, NearPointer => NearPointerOperand}
import assembler.{Encodable, Label, Resource}

abstract class ShortJumpOperation[OffsetType <: X86Offset](val label: Label, val shortOpcode: List[Byte], mnemonic: String, override val target: Label)
                                 (implicit processorMode: ProcessorModeWithOffset[OffsetType])
  extends Resource with RelativeReference {

  val shortJumpSize: Int = shortOpcode.length + 1

  override val minimumSize: Int = shortJumpSize
  override val maximumSize: Int = shortJumpSize

  def encodableForShortPointer(pointer: NearPointerOperand[OffsetType]): Resource with Encodable

  override def toString = s"$labelPrefix$mnemonic $target"

  override def sizeForDistance(distance: Int)(forward: Boolean): Int = shortJumpSize

  override def encodableForDistance(distance: Int)(forward: Boolean): Resource with Encodable = {
    val offset = if (forward) {
      implicitly[ProcessorModeWithOffset[OffsetType]].offset(distance)
    } else {
      implicitly[ProcessorModeWithOffset[OffsetType]].offset(-distance - shortJumpSize)
    }
    assume(offset.isShort)
    encodableForShortPointer(ShortPointer(offset))
  }
}
