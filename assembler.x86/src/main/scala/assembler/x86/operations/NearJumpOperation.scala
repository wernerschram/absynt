package assembler.x86.operations

import assembler.Label
import assembler.ListExtensions._
import assembler.memory.MemoryPage
import assembler.reference.ReferencingInstructionOnPage
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.{NearPointer => NearPointerOperand}

abstract class NearJumpOperation(shortOpcode: List[Byte], longOpcode: List[Byte], mnemonic: String, label: Label)
                                (implicit processorMode: ProcessorMode)
  extends ShortJumpOperation(shortOpcode, mnemonic, label) {

  override def createOperation(thisLocation: Int, targetLocation: Int)
                              (implicit memoryPage: MemoryPage, processorMode: ProcessorMode): ReferencingInstructionOnPage =
    new ShortOrNearJumpInstructionOnPage(shortOpcode, longOpcode, thisLocation, targetLocation)(memoryPage, processorMode)

  def encodeForLongPointer(pointer: NearPointerOperand)
                          (implicit page: MemoryPage): List[Byte]

  class ShortOrNearJumpInstructionOnPage(val shortOpcode: List[Byte], val longOpcode: List[Byte], thisLocation: Int,
                                         destinationLocation: Int)(implicit page: MemoryPage, processorMode: ProcessorMode)
    extends ReferencingInstructionOnPage(thisLocation, destinationLocation) {

    override val minimumSize: Int = shortJumpSize
    override val maximumSize: Int = longJumpSize
    val shortJumpSize: Int = shortOpcode.length + 1
    val longJumpSize: Int = processorMode match {
      case ProcessorMode.Real => longOpcode.length + 2
      case ProcessorMode.Protected | ProcessorMode.Long => longOpcode.length + 4
    }
    val forwardShortLongBoundary = Byte.MaxValue
    val backwardShortLongBoundary: Int = (-Byte.MinValue) - shortJumpSize

    override def getSizeForDistance(forward: Boolean, distance: Int): Int =
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

    override def encodeForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage): List[Byte] = {
      if (forward) {
        if (distance <= forwardShortLongBoundary) {
          encodeForShortPointer(NearPointerOperand(distance.toByte.encodeLittleEndian))
        } else {
          if (processorMode == ProcessorMode.Real) {
            encodeForLongPointer(NearPointerOperand(distance.toShort.encodeLittleEndian))
          } else {
            encodeForLongPointer(NearPointerOperand(distance.encodeLittleEndian))
          }
        }
      } else {
        if (distance <= backwardShortLongBoundary) {
          encodeForShortPointer(NearPointerOperand((-distance - shortJumpSize).toByte.encodeLittleEndian))
        } else {
          if (processorMode == ProcessorMode.Real) {
            encodeForLongPointer(NearPointerOperand((-distance - longJumpSize).toShort.encodeLittleEndian))
          } else {
            encodeForLongPointer(NearPointerOperand((-distance - longJumpSize).encodeLittleEndian))
          }
        }
      }
    }
  }

}
