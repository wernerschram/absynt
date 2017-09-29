package assembler.x86.operations

import assembler.ListExtensions.ByteEncoder
import assembler.reference.RelativeReference
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.{ShortPointer, NearPointer => NearPointerOperand}
import assembler.{Encodable, Label, Resource}

abstract class ShortJumpOperation(val label: Label, val shortOpcode: List[Byte], mnemonic: String, override val target: Label)
                                 (implicit processorMode: ProcessorMode)
  extends Resource with RelativeReference {

  val shortJumpSize: Int = shortOpcode.length + 1

  override val minimumSize: Int = shortJumpSize
  override val maximumSize: Int = shortJumpSize

  def encodableForShortPointer(pointer: NearPointerOperand): Resource with Encodable

  override def toString = s"$labelPrefix$mnemonic $target"

  override def sizeForDistance(distance: Int)(forward: Boolean): Int = shortJumpSize

  override def encodableForDistance(distance: Int)(forward: Boolean): Resource with Encodable = {
    assume(distance > (Byte.MinValue + shortJumpSize))
    assume(distance < Byte.MaxValue)
    if (forward) {
      encodableForShortPointer(ShortPointer(distance.toByte))
    } else {
      encodableForShortPointer(ShortPointer((-distance - shortJumpSize).toByte))
    }
  }
}
