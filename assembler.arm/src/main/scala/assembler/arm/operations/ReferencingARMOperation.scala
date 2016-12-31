package assembler.arm.instructions

import scala.collection.concurrent.TrieMap

import assembler.ListExtensions._

import assembler.Encodable
import assembler.arm.ProcessorMode
import assembler.arm.operands.RelativePointer
import assembler.memory.MemoryPage
import assembler.reference.ReferencingInstruction
import assembler.reference.ReferencingInstructionOnPage
import assembler.Label
import assembler.arm.operations.ARMOperation
import assembler.arm.operations.Conditional
import assembler.arm.operands.Condition.Condition

abstract class ReferencingARMOperation[PointerType](val opcode: String, val label: Label, val condition: Condition, newPointer: (Int) => PointerType)(implicit processorMode: ProcessorMode)
    extends Conditional with ReferencingInstruction {

  class ARMReferencingInstructionOnPage(thisLocation: Int, destinationLocation: Int)(implicit page: MemoryPage, processorMode: ProcessorMode)
      extends ReferencingInstructionOnPage(thisLocation, destinationLocation) {

    val branchSize = 4
    override val minimumSize = branchSize
    override val maximumSize = branchSize

    override def getSizeForDistance(forward: Boolean, distance: Int) = branchSize

    override def encodeForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage) =
      encodeWordForDistance(getPointerForDistance(forward, distance)).encodeLittleEndian

    def getPointerForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage) = {
      if (forward) {
        newPointer(distance - 4)
      } else {
        newPointer(-distance - 8)
      }
    }
  }

  val pageMap = new TrieMap[MemoryPage, ARMReferencingInstructionOnPage]

  def createOperation(thisLocation: Int, targetLocation: Int, memoryPage: MemoryPage, processorMode: ProcessorMode): ARMReferencingInstructionOnPage =
    new ARMReferencingInstructionOnPage(thisLocation, targetLocation)(memoryPage, processorMode)

  def encodeWordForDistance(destination: PointerType)(implicit page: MemoryPage): Int

  override def getOrElseCreateInstruction()(implicit page: MemoryPage): ARMReferencingInstructionOnPage = {
    val target = page.encodableLocation(page.getEncodableByCondition(label))
    pageMap.getOrElseUpdate(page, createOperation(page.encodableLocation(this), target, page, processorMode))
  }

  override def size()(implicit page: MemoryPage) = getOrElseCreateInstruction().size

  override def toString = s"${super.toString()} $label"
}