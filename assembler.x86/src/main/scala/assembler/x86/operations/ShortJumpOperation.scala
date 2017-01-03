package assembler.x86.operations

import assembler.ListExtensions.ByteEncoder
import assembler.memory.MemoryPage
import assembler.reference.{ReferencingInstruction, ReferencingInstructionOnPage}
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.{NearPointer => NearPointerOperand}
import assembler.{Encodable, Label}

import scala.collection.concurrent.TrieMap

abstract class ShortJumpOperation(val shortOpcode: List[Byte], mnemonic: String, label: Label)(implicit processorMode: ProcessorMode)
  extends Encodable() with ReferencingInstruction {

  val pageMap = new TrieMap[MemoryPage, ReferencingInstructionOnPage]

  def encodeForShortPointer(pointer: NearPointerOperand)(implicit page: MemoryPage): List[Byte]

  override def size()(implicit page: MemoryPage): Int = getOrElseCreateInstruction().size

  override def getOrElseCreateInstruction()(implicit page: MemoryPage): ReferencingInstructionOnPage = {
    val target = page.encodableLocation(page.getEncodableByCondition(label))
    pageMap.getOrElseUpdate(page, createOperation(page.encodableLocation(this), target))
  }

  def createOperation(thisLocation: Int, targetLocation: Int)
                     (implicit memoryPage: MemoryPage, processorMode: ProcessorMode): ReferencingInstructionOnPage =
    new ShortJumpInstructionOnPage(shortOpcode, thisLocation, targetLocation)(memoryPage, processorMode)

  override def toString = s"$mnemonic $label"

  class ShortJumpInstructionOnPage(val shortOpcode: List[Byte], thisLocation: Int, destinationLocation: Int)
                                  (implicit page: MemoryPage, processorMode: ProcessorMode)
    extends ReferencingInstructionOnPage(thisLocation, destinationLocation) {

    override val minimumSize: Int = shortJumpSize
    override val maximumSize: Int = shortJumpSize
    val shortJumpSize: Int = shortOpcode.length + 1

    override def getSizeForDistance(forward: Boolean, distance: Int): Int = shortJumpSize

    override def encodeForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage): List[Byte] = {
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
