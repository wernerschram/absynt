package assembler.x86.operations

import assembler.ListExtensions.ByteEncoder
import assembler.reference.ReferencingInstruction
import assembler.sections.Section
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.{NearPointer => NearPointerOperand}
import assembler.{Encodable, Label}

abstract class ShortJumpOperation(val label: Label, val shortOpcode: List[Byte], mnemonic: String, override val target: Label)
                                 (implicit processorMode: ProcessorMode)
  extends Encodable() with ReferencingInstruction {

  val shortJumpSize: Int = shortOpcode.length + 1

  override val minimumSize: Int = shortJumpSize
  override val maximumSize: Int = shortJumpSize

  def encodableForShortPointer(pointer: NearPointerOperand)(implicit page: Section): Encodable

  override def toString = s"$labelPrefix$mnemonic $target"

  override def getSizeForDistance(forward: Boolean, distance: Int)(implicit page: Section): Int = shortJumpSize

  override def encodableForDistance(forward: Boolean, distance: Int)(implicit page: Section): Encodable = {
    assume(distance > (Byte.MinValue + shortJumpSize))
    assume(distance < Byte.MaxValue)
    if (forward) {
      encodableForShortPointer(NearPointerOperand(distance.toByte.encodeLittleEndian))
    } else {
      encodableForShortPointer(NearPointerOperand((-distance - shortJumpSize).toByte.encodeLittleEndian))
    }
  }
}
