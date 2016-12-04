package assembler.arm.instructions

import scala.collection.concurrent.TrieMap

import assembler.Encodable
import assembler.LabelCondition
import assembler.arm.ProcessorMode
import assembler.arm.operands.RelativePointer
import assembler.memory.MemoryPage
import assembler.reference.ReferencingInstruction
import assembler.reference.ReferencingInstructionOnPage

trait ReferencingARMInstructionOnPage extends ReferencingInstructionOnPage {

  def getPointerForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage) = {
    if (forward) {
      RelativePointer(distance - 4)
    } else {
      RelativePointer(-distance - 8)
    }
  }
}

class ReferencingARMInstruction[T <: ReferencingARMInstructionOnPage](
  factory: (Int, Int, MemoryPage, ProcessorMode) => T,
  mnemonic: String,
  condition: LabelCondition)(implicit processorMode: ProcessorMode)
    extends Encodable with ReferencingInstruction[T] {
  val pageMap = new TrieMap[MemoryPage, T]

  override def getOrElseCreateInstruction()(implicit page: MemoryPage): T = {
    val target = page.encodableLocation(page.getEncodableByCondition(condition))
    pageMap.getOrElseUpdate(page, { factory(page.encodableLocation(this), target, page, processorMode) })
  }
  override def size()(implicit page: MemoryPage) = getOrElseCreateInstruction().size

  override def toString = s"$mnemonic $condition"
}