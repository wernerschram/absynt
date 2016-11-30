package assembler.x86.instructions.jump

import assembler.LabelCondition
import assembler.ListExtensions._
import assembler.memory.MemoryPage
import assembler.reference.BranchInstructionOnPage
import assembler.x86.ProcessorMode
import assembler.x86.operations.ReferencingX86Operation
import assembler.x86.operands.memoryaccess.NearPointer
import assembler.x86.operations.{ NearPointer => NearPointerOperation }
import assembler.x86.operations.Static
import assembler.x86.operands.ValueSize

abstract class ShortOrNearRelativeJump(shortOpcode: List[Byte], val nearOpcode: List[Byte], mnemonic: String) extends ShortRelativeJump(shortOpcode, mnemonic) {

  private def Rel16(nearPointer: NearPointer)(implicit processorMode: ProcessorMode) = new Static(nearOpcode, mnemonic) with NearPointerOperation {
    override val pointer = nearPointer: NearPointer

    override def validate = {
      super.validate
      processorMode match {
        case ProcessorMode.Long | ProcessorMode.Protected => assume(pointer.operandByteSize != ValueSize.Word)
        case ProcessorMode.Real => assume(pointer.operandByteSize == ValueSize.Word)
      }
    }
  }

  override def apply(nearPointer: NearPointer)(implicit processorMode: ProcessorMode) = nearPointer.operandByteSize match {
    case ValueSize.Byte =>
      super.apply(nearPointer)
    case _ =>
      Rel16(nearPointer)
  }

  class ShortOrNearJumpInstructionOnPage(thisLocation: Int, destinationLocation: Int)(implicit page: MemoryPage, processorMode: ProcessorMode)
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
            apply(NearPointer(distance.encodeLittleEndian)).encodeByte
          }
        }
      } else {
        if (distance <= backwardShortNearBoundary) {
          apply(NearPointer((-distance - shortJumpSize).toByte.encodeLittleEndian)).encodeByte
        } else {
          if (processorMode == ProcessorMode.Real) {
            apply(NearPointer((-distance - nearJumpSize).toShort.encodeLittleEndian)).encodeByte
          } else {
            apply(NearPointer((-distance - nearJumpSize).encodeLittleEndian)).encodeByte
          }
        }
      }
    }
  }

  override def apply(condition: LabelCondition)(implicit processorMode: ProcessorMode) =
    new ReferencingX86Operation[BranchInstructionOnPage](
      (thisLocation, targetLocation, memoryPage, processorMode) =>
        new ShortOrNearJumpInstructionOnPage(thisLocation, targetLocation)(memoryPage, processorMode),
      mnemonic, condition)
}