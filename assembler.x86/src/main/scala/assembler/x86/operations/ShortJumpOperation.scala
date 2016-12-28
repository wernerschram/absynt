package assembler.x86.operations

import scala.collection.concurrent.TrieMap

import assembler.Encodable
import assembler.LabelCondition
import assembler.ListExtensions.ByteEncoder
import assembler.memory.MemoryPage
import assembler.reference.BranchInstructionOnPage
import assembler.reference.ReferencingInstruction
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.{ NearPointer => NearPointerOperand }

abstract class ShortJumpOperation(val shortOpcode: List[Byte], mnemonic: String, condition: LabelCondition)(implicit processorMode: ProcessorMode)
    extends Encodable() with ReferencingInstruction[BranchInstructionOnPage] {

  class ShortJumpInstructionOnPage(val shortOpcode: List[Byte], thisLocation: Int, destinationLocation: Int)(implicit page: MemoryPage, processorMode: ProcessorMode)
      extends BranchInstructionOnPage(thisLocation, destinationLocation) {

    val shortJumpSize = shortOpcode.length + 1
    override val minimumSize = shortJumpSize
    override val maximumSize = shortJumpSize

    override def getSizeForDistance(forward: Boolean, distance: Int) = shortJumpSize

    override def encodeForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage) =
      {
        assume(distance > (Byte.MinValue + shortJumpSize))
        assume(distance < Byte.MaxValue)
        if (forward) {
          encodeForPointer(NearPointerOperand(distance.toByte.encodeLittleEndian))
        } else {
          encodeForPointer(NearPointerOperand((-distance - shortJumpSize).toByte.encodeLittleEndian))
        }
      }
  }

  val pageMap = new TrieMap[MemoryPage, BranchInstructionOnPage]

  def createOperation(thisLocation: Int, targetLocation: Int)(implicit memoryPage: MemoryPage, processorMode: ProcessorMode): BranchInstructionOnPage =
    new ShortJumpInstructionOnPage(shortOpcode, thisLocation, targetLocation)(memoryPage, processorMode)

  override def getOrElseCreateInstruction()(implicit page: MemoryPage): BranchInstructionOnPage = {
    val target = page.encodableLocation(page.getEncodableByCondition(condition))
    pageMap.getOrElseUpdate(page, createOperation(page.encodableLocation(this), target))
  }

  def encodeForPointer(pointer: NearPointerOperand)(implicit page: MemoryPage): List[Byte]

  override def size()(implicit page: MemoryPage) = getOrElseCreateInstruction().size

  override def toString() = s"${mnemonic} ${condition}"
}
