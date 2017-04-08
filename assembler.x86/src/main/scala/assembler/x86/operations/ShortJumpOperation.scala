package assembler.x86.operations

import assembler.ListExtensions.ByteEncoder
import assembler.sections.Section
import assembler.reference.{ReferencingInstruction, ReferencingInstructionOnPage}
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.{NearPointer => NearPointerOperand}
import assembler.{Encodable, Label}

import scala.collection.concurrent.TrieMap

abstract class ShortJumpOperation(val label: Label, val shortOpcode: List[Byte], mnemonic: String, override val target: Label)
                                 (implicit processorMode: ProcessorMode)
  extends Encodable() with ReferencingInstruction {

  val pageMap = new TrieMap[Section, ReferencingInstructionOnPage]

  def encodeForShortPointer(pointer: NearPointerOperand)(implicit page: Section): List[Byte]

  override def size()(implicit page: Section): Int = getOrElseCreateInstruction().size

  override def getOrElseCreateInstruction()(implicit page: Section): ReferencingInstructionOnPage = {
    pageMap.getOrElseUpdate(page, createOperation(this, target))
  }

  def createOperation(thisOperation: ReferencingInstruction, destination: Label)
                     (implicit section: Section, processorMode: ProcessorMode): ReferencingInstructionOnPage =
    new ShortJumpInstructionOnPage(shortOpcode, thisOperation, destination)(section, processorMode)

  override def toString = s"$mnemonic $target"

  class ShortJumpInstructionOnPage(val shortOpcode: List[Byte], thisOperation: ReferencingInstruction, destination: Label)
                                  (implicit page: Section, processorMode: ProcessorMode)
    extends ReferencingInstructionOnPage(thisOperation, destination) {

    val shortJumpSize: Int = shortOpcode.length + 1
    override val minimumSize: Int = shortJumpSize
    override val maximumSize: Int = shortJumpSize

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
