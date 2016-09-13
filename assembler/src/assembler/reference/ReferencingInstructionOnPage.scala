package assembler.reference

import assembler.MemoryPage

abstract class ReferencingInstructionOnPage()(implicit page: MemoryPage) {
 
  def minimumEstimatedSize: Int
  def maximumEstimatedSize: Int
  
  def sizeIsKnown: Boolean

  def estimatedSize(sizeAssumptions: Map[ReferencingInstructionOnPage, Int]): Int
  
  def size: Int
  
  def encodeByte: List[Byte]
}