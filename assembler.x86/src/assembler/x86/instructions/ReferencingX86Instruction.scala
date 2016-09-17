package assembler.x86.instructions

import scala.collection.concurrent.TrieMap

import assembler.LabelCondition
import assembler.memory.MemoryPage
import assembler.reference.ReferencingInstruction
import assembler.reference.ReferencingInstructionOnPage
import assembler.x86.ProcessorMode

class ReferencingX86Instruction[T <: ReferencingInstructionOnPage](
  factory: (Int, Int, MemoryPage, ProcessorMode) => T,
  mnemonic: String,
  condition: LabelCondition)(implicit processorMode: ProcessorMode)
    extends X86Instruction() with ReferencingInstruction[T] {
  val pageMap = new TrieMap[MemoryPage, T]

  override def getOrElseCreateInstruction()(implicit page: MemoryPage): T = {
    val target = page.encodableLocation(page.getInstructionByCondition(condition))
    pageMap.getOrElseUpdate(page, { factory(page.encodableLocation(this), target, page, processorMode) })
  }

  override def size()(implicit page: MemoryPage) = getOrElseCreateInstruction().size
  
  override def toString() = s"${mnemonic} ${condition}"
}