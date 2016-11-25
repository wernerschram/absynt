package assembler.x86.operations

import scala.collection.concurrent.TrieMap
import assembler.Encodable
import assembler.LabelCondition
import assembler.memory.MemoryPage
import assembler.reference.ReferencingInstruction
import assembler.reference.ReferencingInstructionOnPage
import assembler.x86.ProcessorMode

class ReferencingX86Operation[T <: ReferencingInstructionOnPage](
  factory: (Int, Int, MemoryPage, ProcessorMode) => T,
  mnemonic: String,
  condition: LabelCondition)(implicit processorMode: ProcessorMode)
    extends Encodable() with ReferencingInstruction[T] {
  val pageMap = new TrieMap[MemoryPage, T]

  override def getOrElseCreateInstruction()(implicit page: MemoryPage): T = {
    val target = page.encodableLocation(page.getEncodableByCondition(condition))
    pageMap.getOrElseUpdate(page, { factory(page.encodableLocation(this), target, page, processorMode) })
  }

  override def size()(implicit page: MemoryPage) = getOrElseCreateInstruction().size

  override def toString() = s"${mnemonic} ${condition}"
}
