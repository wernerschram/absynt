package assembler.x86

import assembler.MemoryPage

abstract class ReferencingInstructionOnPage()(implicit page: MemoryPage, processorMode: ProcessorMode) {
 
  def minimumEstimatedSize: Int
  def maximumEstimatedSize: Int
  
  def sizeIsKnown: Boolean

  def estimatedSize(sizeAssumptions: Map[ReferencingInstructionOnPage, Int]): Int
  
  def size: Int
  
  def encodeByte: List[Byte]
}