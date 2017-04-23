package assembler.x86.operations

import assembler.ListExtensions.ByteEncoder
import assembler.sections.Section
import assembler.reference.{ReferencingInstruction, ReferencingInstructionOnPage}
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.{NearPointer => NearPointerOperand}
import assembler.{Encodable, Label}

abstract class ShortJumpOperation(val label: Label, val shortOpcode: List[Byte], mnemonic: String, override val target: Label)
                                 (implicit processorMode: ProcessorMode)
  extends Encodable() with ReferencingInstruction {

  val shortJumpSize: Int = shortOpcode.length + 1

  override val minimumSize: Int = shortJumpSize
  override val maximumSize: Int = shortJumpSize

  def encodeForShortPointer(pointer: NearPointerOperand)(implicit page: Section): List[Byte]

  override def size()(implicit page: Section): Int = getOrElseCreateInstruction().size

  override def createOperation(thisOperation: ReferencingInstruction, destination: Label, section: Section): ReferencingInstructionOnPage =
    new ShortJumpInstructionOnPage(shortOpcode, thisOperation, destination)(section, processorMode)

  override def toString = s"$mnemonic $target"

  class ShortJumpInstructionOnPage(val shortOpcode: List[Byte], thisOperation: ReferencingInstruction, destination: Label)
                                  (implicit page: Section, processorMode: ProcessorMode)
    extends ReferencingInstructionOnPage(thisOperation, destination) {

    override def getSizeForDistance(forward: Boolean, distance: Int): Int = shortJumpSize

    override def encodeForDistance(forward: Boolean, distance: Int)(implicit page: Section): List[Byte] = {
      assume(distance > (Byte.MinValue + shortJumpSize))
      assume(distance < Byte.MaxValue)
      if (forward) {
        encodeForShortPointer(NearPointerOperand(distance.toByte.encodeLittleEndian))
      } else {
        encodeForShortPointer(NearPointerOperand((-distance - shortJumpSize).toByte.encodeLittleEndian))
      }
    }
  }

}
