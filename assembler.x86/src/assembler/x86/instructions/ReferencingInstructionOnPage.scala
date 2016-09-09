import assembler.x86.ProcessorMode
import assembler.MemoryPage

abstract class ReferencingInstructionOnPage()(implicit page: MemoryPage, processorMode: ProcessorMode) {
 
  def minimumEstimatedSize: Int
  def maximumEstimatedSize: Int
  
  def sizeIsKnown: Boolean

  def estimatedSize(sizeAssumptions: Map[ReferencingInstructionOnPage, Int]): Int
  
  def size: Int
  
  def encode: List[Byte]
}