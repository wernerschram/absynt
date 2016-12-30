package assembler.x86.operations

import scala.collection.concurrent.TrieMap

import assembler.Encodable
import assembler.ListExtensions.ByteEncoder
import assembler.memory.MemoryPage
import assembler.reference.ReferencingInstructionOnPage
import assembler.reference.ReferencingInstruction
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.{ NearPointer => NearPointerOperand }
import assembler.Label

abstract class ShortJumpOperation(val shortOpcode: List[Byte], mnemonic: String, label: Label)(implicit processorMode: ProcessorMode)
    extends Encodable() with ReferencingInstruction {

  class ShortJumpInstructionOnPage(val shortOpcode: List[Byte], thisLocation: Int, destinationLocation: Int)(implicit page: MemoryPage, processorMode: ProcessorMode)
      extends ReferencingInstructionOnPage(thisLocation, destinationLocation) {

    val shortJumpSize = shortOpcode.length + 1
    override val minimumSize = shortJumpSize
    override val maximumSize = shortJumpSize

    override def getSizeForDistance(forward: Boolean, distance: Int) = shortJumpSize

    override def encodeForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage) =
      {
        assume(distance > (Byte.MinValue + shortJumpSize))
        assume(distance < Byte.MaxValue)
        if (forward) {
          encodeForShortPointer(NearPointerOperand(distance.toByte.encodeLittleEndian))
        } else {
          encodeForShortPointer(NearPointerOperand((-distance - shortJumpSize).toByte.encodeLittleEndian))
        }
      }
  }

  val pageMap = new TrieMap[MemoryPage, ReferencingInstructionOnPage]

  def createOperation(thisLocation: Int, targetLocation: Int)(implicit memoryPage: MemoryPage, processorMode: ProcessorMode): ReferencingInstructionOnPage =
    new ShortJumpInstructionOnPage(shortOpcode, thisLocation, targetLocation)(memoryPage, processorMode)

  override def getOrElseCreateInstruction()(implicit page: MemoryPage): ReferencingInstructionOnPage = {
    val target = page.encodableLocation(page.getEncodableByCondition(label))
    pageMap.getOrElseUpdate(page, createOperation(page.encodableLocation(this), target))
  }

  def encodeForShortPointer(pointer: NearPointerOperand)(implicit page: MemoryPage): List[Byte]

  override def size()(implicit page: MemoryPage) = getOrElseCreateInstruction().size

  override def toString() = s"${mnemonic} ${label}"
}
