package assembler.x86.operations

import assembler.{Encodable, Label}
import assembler.ListExtensions._
import assembler.sections.Section
import assembler.reference.ReferencingInstructionOnPage
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.{NearPointer => NearPointerOperand}

abstract class NearJumpOperation(shortOpcode: List[Byte], longOpcode: List[Byte], mnemonic: String, label: Label)
                                (implicit processorMode: ProcessorMode)
  extends ShortJumpOperation(shortOpcode, mnemonic, label) {

  override def createOperation(thisOperation: Encodable, destination: Label)
                              (implicit section: Section, processorMode: ProcessorMode): ReferencingInstructionOnPage =
    new ShortOrNearJumpInstructionOnPage(shortOpcode, longOpcode, thisOperation, destination)(section, processorMode)

  def encodeForLongPointer(pointer: NearPointerOperand)
                          (implicit page: Section): List[Byte]

  class ShortOrNearJumpInstructionOnPage(val shortOpcode: List[Byte], val longOpcode: List[Byte], thisOperation: Encodable,
                                         destination: Label)(implicit page: Section, processorMode: ProcessorMode)
    extends ReferencingInstructionOnPage(thisOperation, destination) {

    val shortJumpSize: Int = shortOpcode.length + 1
    val longJumpSize: Int = processorMode match {
      case ProcessorMode.Real => longOpcode.length + 2
      case ProcessorMode.Protected | ProcessorMode.Long => longOpcode.length + 4
    }
    val forwardShortLongBoundary = Byte.MaxValue
    val backwardShortLongBoundary: Int = (-Byte.MinValue) - shortJumpSize

    override val minimumSize: Int = shortJumpSize
    override val maximumSize: Int = longJumpSize

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

    override def encodeForDistance(forward: Boolean, distance: Int)(implicit page: Section): List[Byte] = {
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
