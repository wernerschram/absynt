package assembler.x86.instructions

import assembler.x86.ProcessorMode
import scala.collection.concurrent.TrieMap
import assembler.Encodable
import assembler.MemoryPage
import assembler.PageLocation
import assembler.Label
import assembler.LabeledEncodable
import assembler.LabelCondition
import assembler.x86.ReferencingInstructionOnPage

abstract class FixedSizeX86Instruction() extends X86Instruction() {
  def size()(implicit page: MemoryPage) = encodeByte().length
  def withLabel(label: Label): LabeledEncodable = new LabeledX86Instruction(this, label)
}

class LabeledX86Instruction(instruction: X86Instruction, override val label: Label) extends FixedSizeX86Instruction with LabeledEncodable {
  override def size()(implicit page: MemoryPage) = instruction.size()
  override def encodeByte()(implicit page: MemoryPage): List[Byte] = instruction.encodeByte()
    
  override def toString() = s"${label.toString}: ${instruction.toString()}"
  
}

abstract class ReferencingX86Instruction[T <: ReferencingInstructionOnPage]()
    extends X86Instruction() {
  def getOrElseCreateInstruction()(implicit page: MemoryPage): T

  def minimumEstimatedSize()(implicit page: MemoryPage) = getOrElseCreateInstruction.minimumEstimatedSize
  def maximumEstimatedSize()(implicit page: MemoryPage) = getOrElseCreateInstruction.maximumEstimatedSize

  def sizeIsKnown()(implicit page: MemoryPage) = getOrElseCreateInstruction.sizeIsKnown

  def estimatedSize(sizeAssumptions: Map[ReferencingInstructionOnPage, Int])(implicit page: MemoryPage) = getOrElseCreateInstruction.estimatedSize(sizeAssumptions)

  def size()(implicit page: MemoryPage) = getOrElseCreateInstruction.size
  def encodeByte()(implicit page: MemoryPage) = getOrElseCreateInstruction.encodeByte 
}

class DeferedReferencingX86Instruction[T <: ReferencingInstructionOnPage](
    factory: (PageLocation, PageLocation, MemoryPage, ProcessorMode)=>T,
    mnemonic: String,
    condition: LabelCondition
    )(implicit processorMode: ProcessorMode)
    extends ReferencingX86Instruction[T]() {
  val pageMap = new TrieMap[MemoryPage, T]

  override def getOrElseCreateInstruction()(implicit page: MemoryPage): T = {
    val target = page.encodableLocation(page.getInstructionByCondition(condition))
    pageMap.getOrElseUpdate(page, { factory(page.encodableLocation(this), target, page, processorMode) })
  }
  
  override def withLabel(label: Label) = new LabeledReferencingX86Instruction[T](this, label)

  override def toString() = s"${mnemonic} ${condition}"
}

class LabeledReferencingX86Instruction[T <: ReferencingInstructionOnPage](
    instruction: ReferencingX86Instruction[T],
    val label: Label) extends ReferencingX86Instruction[T]() with LabeledEncodable {
  override def getOrElseCreateInstruction()(implicit page: MemoryPage): T = instruction.getOrElseCreateInstruction()

  override def size()(implicit page: MemoryPage) = instruction.size()
  override def encodeByte()(implicit page: MemoryPage): List[Byte] = instruction.encodeByte()
  override def withLabel(label: Label) = new LabeledReferencingX86Instruction[T](this, label)
}