package assembler.arm.instructions

import scala.collection.concurrent.TrieMap

import assembler.ListExtensions._

import assembler.Encodable
import assembler.LabelCondition
import assembler.arm.ProcessorMode
import assembler.arm.operands.RelativePointer
import assembler.memory.MemoryPage
import assembler.reference.ReferencingInstruction
import assembler.reference.ReferencingInstructionOnPage
import assembler.reference.BranchInstructionOnPage

abstract class ReferencingARMInstruction[PointerType <: RelativePointer](mnemonic: String, condition: LabelCondition, newPointer: (Int) => PointerType)(implicit processorMode: ProcessorMode)
    extends Encodable with ReferencingInstruction[BranchInstructionOnPage] {

  class ARMBranchInstructionOnPage(thisLocation: Int, destinationLocation: Int)(implicit page: MemoryPage, processorMode: ProcessorMode)
      extends BranchInstructionOnPage(thisLocation, destinationLocation) {

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

  val pageMap = new TrieMap[MemoryPage, ARMBranchInstructionOnPage]

  def createOperation(thisLocation: Int, targetLocation: Int, memoryPage: MemoryPage, processorMode: ProcessorMode): ARMBranchInstructionOnPage =
    new ARMBranchInstructionOnPage(thisLocation, targetLocation)(memoryPage, processorMode)

  def encodeWordForDistance(destination: PointerType)(implicit page: MemoryPage): Int

  override def getOrElseCreateInstruction()(implicit page: MemoryPage): ARMBranchInstructionOnPage = {
    val target = page.encodableLocation(page.getEncodableByCondition(condition))
    pageMap.getOrElseUpdate(page, createOperation(page.encodableLocation(this), target, page, processorMode))
  }
  override def size()(implicit page: MemoryPage) = getOrElseCreateInstruction().size

  override def toString = s"$mnemonic $condition"
}