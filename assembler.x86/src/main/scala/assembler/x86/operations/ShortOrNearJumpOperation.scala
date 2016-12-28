package assembler.x86.operations

import assembler.LabelCondition
import assembler.ListExtensions._
import assembler.memory.MemoryPage
import assembler.reference.BranchInstructionOnPage
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.{ NearPointer => NearPointerOperand }

abstract class ShortOrNearJumpOperation(shortOpcode: List[Byte], longOpcode: List[Byte], mnemonic: String, condition: LabelCondition)(implicit processorMode: ProcessorMode)
    extends ShortJumpOperation(shortOpcode, mnemonic, condition) {

  class ShortOrNearJumpInstructionOnPage(val shortOpcode: List[Byte], val longOpcode: List[Byte], thisLocation: Int, destinationLocation: Int)(implicit page: MemoryPage, processorMode: ProcessorMode)
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
          encodeForPointer(NearPointerOperand(distance.toByte.encodeLittleEndian))
        } else {
          if (processorMode == ProcessorMode.Real) {
            encodeForNearPointer(NearPointerOperand(distance.toShort.encodeLittleEndian))
          } else {
            encodeForNearPointer(NearPointerOperand(distance.encodeLittleEndian))
          }
        }
      } else {
        if (distance <= backwardShortLongBoundary) {
          encodeForPointer(NearPointerOperand((-distance - shortJumpSize).toByte.encodeLittleEndian))
        } else {
          if (processorMode == ProcessorMode.Real) {
            encodeForNearPointer(NearPointerOperand((-distance - longJumpSize).toShort.encodeLittleEndian))
          } else {
            encodeForNearPointer(NearPointerOperand((-distance - longJumpSize).encodeLittleEndian))
          }
        }
      }
    }
  }

  override def createOperation(thisLocation: Int, targetLocation: Int)(implicit memoryPage: MemoryPage, processorMode: ProcessorMode): BranchInstructionOnPage =
    new ShortOrNearJumpInstructionOnPage(shortOpcode, longOpcode, thisLocation, targetLocation)(memoryPage, processorMode)

  def encodeForNearPointer(pointer: NearPointerOperand)(implicit page: MemoryPage): List[Byte]
}
