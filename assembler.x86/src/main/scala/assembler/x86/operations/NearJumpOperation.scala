package assembler.x86.operations

import assembler.{Encodable, Label}
import assembler.ListExtensions._
import assembler.sections.Section
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.{NearPointer => NearPointerOperand}

abstract class NearJumpOperation(label: Label, shortOpcode: List[Byte], longOpcode: List[Byte], mnemonic: String, target: Label)
                                (implicit processorMode: ProcessorMode)
  extends ShortJumpOperation(label, shortOpcode, mnemonic, target) {

  val forwardShortLongBoundary = Byte.MaxValue
  val backwardShortLongBoundary: Int = (-Byte.MinValue) - shortJumpSize

  val longJumpSize: Int = processorMode match {
    case ProcessorMode.Real => longOpcode.length + 2
    case ProcessorMode.Protected | ProcessorMode.Long => longOpcode.length + 4
  }

  override val maximumSize: Int = longJumpSize

  def encodableForLongPointer(pointer: NearPointerOperand)(implicit page: Section): Encodable

  override def getSizeForDistance(forward: Boolean, distance: Int)(implicit page: Section): Int =
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

  override def encodableForDistance(forward: Boolean, distance: Int)(implicit page: Section): Encodable = {
    if (forward) {
      if (distance <= forwardShortLongBoundary) {
        encodableForShortPointer(NearPointerOperand(distance.toByte.encodeLittleEndian))
      } else {
        if (processorMode == ProcessorMode.Real) {
          encodableForLongPointer(NearPointerOperand(distance.toShort.encodeLittleEndian))
        } else {
          encodableForLongPointer(NearPointerOperand(distance.encodeLittleEndian))
        }
      }
    } else {
      if (distance <= backwardShortLongBoundary) {
        encodableForShortPointer(NearPointerOperand((-distance - shortJumpSize).toByte.encodeLittleEndian))
      } else {
        if (processorMode == ProcessorMode.Real) {
          encodableForLongPointer(NearPointerOperand((-distance - longJumpSize).toShort.encodeLittleEndian))
        } else {
          encodableForLongPointer(NearPointerOperand((-distance - longJumpSize).encodeLittleEndian))
        }
      }
    }
  }
}
