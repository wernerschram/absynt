package assembler.x86.instructions.jump

import assembler.LabelCondition
import assembler.ListExtensions._
import assembler.memory.MemoryPage
import assembler.reference.BranchInstructionOnPage
import assembler.x86.ProcessorMode
import assembler.x86.operations.ReferencingX86Operation
import assembler.x86.operands.memoryaccess.NearPointer
import assembler.x86.operations.Static
import assembler.x86.operations.{NearPointer => NearPointerOperation}

abstract class ShortRelativeJump(val shortOpcode: List[Byte], implicit val mnemonic: String) {

  private def Rel8(nearPointer: NearPointer)(implicit processorMode: ProcessorMode) = new Static(shortOpcode, mnemonic) with NearPointerOperation {
    override val pointer = nearPointer
  }

  def apply(nearPointer: NearPointer)(implicit processorMode: ProcessorMode) = {
    assume(nearPointer.operandByteSize == 1)
    Rel8(nearPointer)
  }

  class ShortJumpInstructionOnPage(thisLocation: Int, destinationLocation: Int)(implicit page: MemoryPage, processorMode: ProcessorMode)
      extends BranchInstructionOnPage(thisLocation, destinationLocation) {

    val shortJumpSize = shortOpcode.length + 1
    override val minimumSize = shortJumpSize
    override val maximumSize = shortJumpSize

    override def getSizeForDistance(forward: Boolean, distance: Int) = shortJumpSize

    override def encodeForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage) = {
      assume(distance > (Byte.MinValue + shortJumpSize))
      assume(distance < Byte.MaxValue)
      if (forward) {
        apply(NearPointer(distance.toByte.encodeLittleEndian)).encodeByte
      } else {
        apply(NearPointer((-distance - shortJumpSize).toByte.encodeLittleEndian)).encodeByte
      }
    }
  }

  def apply(condition: LabelCondition)(implicit processorMode: ProcessorMode) =
    new ReferencingX86Operation[BranchInstructionOnPage](
      (thisLocation, targetLocation, memoryPage, processorMode) =>
        new ShortJumpInstructionOnPage(thisLocation, targetLocation)(memoryPage, processorMode),
        mnemonic, condition)
}