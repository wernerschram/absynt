package assembler.arm.instructions

import scala.collection.concurrent.TrieMap
import assembler.LabelCondition
import assembler.MemoryPage
import assembler.PageLocation
import assembler.reference.ReferencingInstruction
import assembler.reference.ReferencingInstructionOnPage
import assembler.arm.ProcessorMode
import assembler.Encodable

class ReferencingARMInstruction[T <: ReferencingInstructionOnPage](
  factory: (PageLocation, PageLocation, MemoryPage, ProcessorMode) => T,
  mnemonic: String,
  condition: LabelCondition)(implicit processorMode: ProcessorMode)
    extends Encodable() with ReferencingInstruction[T] {
  val pageMap = new TrieMap[MemoryPage, T]

  override def getOrElseCreateInstruction()(implicit page: MemoryPage): T = {
    val target = page.encodableLocation(page.getInstructionByCondition(condition))
    pageMap.getOrElseUpdate(page, { factory(page.encodableLocation(this), target, page, processorMode) })
  }
  override def size()(implicit page: MemoryPage) = getOrElseCreateInstruction().size
  
  override def toString() = s"${mnemonic} ${condition}"
}