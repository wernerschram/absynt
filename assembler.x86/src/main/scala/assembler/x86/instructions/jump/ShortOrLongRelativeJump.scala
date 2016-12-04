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

abstract class ShortOrLongRelativeJump(shortOpcode: List[Byte], val longOpcode: List[Byte], mnemonic: String) extends ShortRelativeJump(shortOpcode, mnemonic) {

  private def Rel16(nearPointer: NearPointer)(implicit processorMode: ProcessorMode) = new Static(longOpcode, mnemonic) with NearPointerOperation {
    override val pointer = nearPointer

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

  class ShortOrLongJumpInstructionOnPage(thisLocation: Int, destinationLocation: Int)(implicit page: MemoryPage, processorMode: ProcessorMode)
      extends BranchInstructionOnPage(thisLocation, destinationLocation) {
    val shortJumpSize = shortOpcode.length + 1
    val longJumpSize = processorMode match {
      case ProcessorMode.Real => longOpcode.length + 2
      case ProcessorMode.Protected | ProcessorMode.Long => longOpcode.length + 4
    }

    override val minimumSize = shortJumpSize
    override val maximumSize = longJumpSize

    val forwardShortLongBoundary = Byte.MaxValue
    val backwardShortLongBoundary = (-Byte.MinValue) - shortJumpSize

    override def getSizeForDistance(forward: Boolean, distance: Int) =
      if (forward) {
        if (distance <= forwardShortLongBoundary)
          shortJumpSize
        else
          longJumpSize
      } else {
        if (distance <= backwardShortLongBoundary)
          shortJumpSize
        else
          longJumpSize
      }

    override def encodeForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage) = {
      if (forward) {
        if (distance <= forwardShortLongBoundary) {
          apply(NearPointer(distance.toByte.encodeLittleEndian)).encodeByte
        } else {
          if (processorMode == ProcessorMode.Real) {
            apply(NearPointer(distance.toShort.encodeLittleEndian)).encodeByte
          } else {
            apply(NearPointer(distance.encodeLittleEndian)).encodeByte
          }
        }
      } else {
        if (distance <= backwardShortLongBoundary) {
          apply(NearPointer((-distance - shortJumpSize).toByte.encodeLittleEndian)).encodeByte
        } else {
          if (processorMode == ProcessorMode.Real) {
            apply(NearPointer((-distance - longJumpSize).toShort.encodeLittleEndian)).encodeByte
          } else {
            apply(NearPointer((-distance - longJumpSize).encodeLittleEndian)).encodeByte
          }
        }
      }
    }
  }

  override def apply(condition: LabelCondition)(implicit processorMode: ProcessorMode) =
    new ReferencingX86Operation[BranchInstructionOnPage](
      (thisLocation, targetLocation, memoryPage, processorMode) =>
        new ShortOrLongJumpInstructionOnPage(thisLocation, targetLocation)(memoryPage, processorMode),
      mnemonic, condition)
}