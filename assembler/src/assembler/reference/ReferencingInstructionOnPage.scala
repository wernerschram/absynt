package assembler.reference

import assembler.memory.MemoryPage

trait ReferencingInstructionOnPage {

  def minimumEstimatedSize: Int
  def maximumEstimatedSize: Int

  def isEstimated: Boolean

  def estimatedSize(sizeAssumptions: Map[ReferencingInstructionOnPage, Int]): Int

  def size: Int

  def encodeByte: List[Byte]
}
