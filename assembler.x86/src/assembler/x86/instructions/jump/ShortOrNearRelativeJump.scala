package assembler.x86.instructions.jump

import assembler.LabelCondition
import assembler.ListExtensions._
import assembler.memory.PageLocation
import assembler.memory.MemoryPage
import assembler.reference.BranchInstructionOnPage
import assembler.x86.ProcessorMode
import assembler.x86.instructions.ReferencingX86Instruction
import assembler.x86.opcodes.Static
import assembler.x86.operands.memoryaccess.NearPointer

abstract class ShortOrNearRelativeJump(shortOpcode: List[Byte], val nearOpcode: List[Byte], mnemonic: String) extends ShortRelativeJump(shortOpcode, mnemonic) {
  val validate: PartialFunction[(NearPointer, ProcessorMode), Boolean] = {
    case (pointer, ProcessorMode.Long) if (pointer.operandByteSize == 2) => false
    case (pointer, ProcessorMode.Protected) if (pointer.operandByteSize == 2) => false
    case (pointer, ProcessorMode.Real) if (pointer.operandByteSize != 2) => false
    case _ => true
  }

  private val Rel16 = new Static(nearOpcode)(mnemonic).withNearPointer(validate)

  override def apply(nearPointer: NearPointer)(implicit processorMode: ProcessorMode) = nearPointer.operandByteSize match {
    case 1 =>
      super.apply(nearPointer)
    case _ =>
      Rel16(nearPointer)
  }

  class ShortOrNearJumpInstructionOnPage(thisLocation: PageLocation, destinationLocation: PageLocation)(implicit page: MemoryPage, processorMode: ProcessorMode)
      extends BranchInstructionOnPage(thisLocation, destinationLocation) {
    val shortJumpSize = shortOpcode.length + 1
    val nearJumpSize = processorMode match {
      case ProcessorMode.Real => nearOpcode.length + 2
      case ProcessorMode.Protected | ProcessorMode.Long => nearOpcode.length + 4
    }

    override val minimumSize = shortJumpSize
    override val maximumSize = nearJumpSize

    val forwardShortNearBoundary = Byte.MaxValue
    val backwardShortNearBoundary = (-Byte.MinValue) - shortJumpSize

    override def getSizeForDistance(forward: Boolean, distance: Int) =
      if (forward) {
        if (distance <= forwardShortNearBoundary)
          shortJumpSize
        else
          nearJumpSize
      } else {
        if (distance <= backwardShortNearBoundary)
          shortJumpSize
        else
          nearJumpSize
      }

    override def encodeForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage) = {
      if (forward) {
        if (distance <= forwardShortNearBoundary) {
          apply(NearPointer(distance.toByte.encodeLittleEndian)).encodeByte
        } else {
          if (processorMode == ProcessorMode.Real) {
            apply(NearPointer(distance.toShort.encodeLittleEndian)).encodeByte
          } else {
            apply(NearPointer(distance.toInt.encodeLittleEndian)).encodeByte
          }
        }
      } else {
        if (distance <= backwardShortNearBoundary) {
          apply(NearPointer((-distance - shortJumpSize).toByte.encodeLittleEndian)).encodeByte
        } else {
          if (processorMode == ProcessorMode.Real) {
            apply(NearPointer((-distance - nearJumpSize).toShort.encodeLittleEndian)).encodeByte
          } else {
            apply(NearPointer((-distance - nearJumpSize).toInt.encodeLittleEndian)).encodeByte
          }
        }
      }
    }
  }
  
  override def apply(condition: LabelCondition)(implicit processorMode: ProcessorMode) =
    new ReferencingX86Instruction[BranchInstructionOnPage](
      (thisLocation, targetLocation, memoryPage, processorMode) =>  
        new ShortOrNearJumpInstructionOnPage(thisLocation, targetLocation)(memoryPage, processorMode), 
        mnemonic, condition)
}