package assembler.x86.instructions.jump

import assembler.LabelCondition
import assembler.ListExtensions._
import assembler.MemoryPage
import assembler.PageLocation
import assembler.reference.BranchInstructionOnPage
import assembler.x86.ProcessorMode
import assembler.x86.instructions.ReferencingX86Instruction
import assembler.x86.opcodes.Static
import assembler.x86.operands.memoryaccess.NearPointer

abstract class ShortRelativeJump(val shortOpcode: List[Byte], implicit val mnemonic: String) {

  private val Rel8 = new Static(shortOpcode).withNearPointer()

  def apply(nearPointer: NearPointer)(implicit processorMode: ProcessorMode) = {
    assume(nearPointer.operandByteSize == 1)
    Rel8(nearPointer)
  }

  class ShortJumpInstructionOnPage(thisLocation: PageLocation, destinationLocation: PageLocation)(implicit page: MemoryPage, processorMode: ProcessorMode)
      extends BranchInstructionOnPage(thisLocation, destinationLocation) {
    
    val shortJumpSize = shortOpcode.length + 1
    override val minimumSize = shortJumpSize
    override val maximumSize = shortJumpSize

    override def getSizeForDistance(forward: Boolean, distance: Int) = shortJumpSize

    override def encodeForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage) = {
      if (forward) {
        apply(NearPointer(distance.toByte.encodeLittleEndian)).encodeByte
      } else {
        apply(NearPointer((-distance - shortJumpSize).toByte.encodeLittleEndian)).encodeByte
      }
    }
  }

  def apply(condition: LabelCondition)(implicit processorMode: ProcessorMode) =
    new ReferencingX86Instruction[BranchInstructionOnPage](
      (thisLocation, targetLocation, memoryPage, processorMode) =>  
        new ShortJumpInstructionOnPage(thisLocation, targetLocation)(memoryPage, processorMode), 
        mnemonic, condition)
}