package assembler.x86.operations

import assembler.ListExtensions.ByteEncoder
import assembler.sections.Section
import assembler.reference.{ReferencingInstruction, ReferencingInstructionOnPage}
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.{NearPointer => NearPointerOperand}
import assembler.{Encodable, Label}

import scala.collection.concurrent.TrieMap

abstract class ShortJumpOperation(val shortOpcode: List[Byte], mnemonic: String, label: Label)(implicit processorMode: ProcessorMode)
  extends Encodable() with ReferencingInstruction {

  val pageMap = new TrieMap[Section, ReferencingInstructionOnPage]

  def encodeForShortPointer(pointer: NearPointerOperand)(implicit page: Section): List[Byte]

  override def size()(implicit page: Section): Int = getOrElseCreateInstruction().size

  override def getOrElseCreateInstruction()(implicit page: Section): ReferencingInstructionOnPage = {
    pageMap.getOrElseUpdate(page, createOperation(this, label))
  }

  def createOperation(thisOperation: Encodable, destination: Label)
                     (implicit section: Section, processorMode: ProcessorMode): ReferencingInstructionOnPage =
    new ShortJumpInstructionOnPage(shortOpcode, thisOperation, destination)(section, processorMode)

  override def toString = s"$mnemonic $label"

  class ShortJumpInstructionOnPage(val shortOpcode: List[Byte], thisOperation: Encodable, destination: Label)
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
