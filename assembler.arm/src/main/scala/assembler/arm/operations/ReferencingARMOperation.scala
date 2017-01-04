package assembler.arm.operations

import assembler.Label
import assembler.ListExtensions._
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition.Condition
import assembler.sections.Section
import assembler.reference.{ReferencingInstruction, ReferencingInstructionOnPage}

import scala.collection.concurrent.TrieMap

abstract class ReferencingARMOperation[PointerType](val opcode: String, val label: Label, val condition: Condition,
                                                    newPointer: (Int) => PointerType)(implicit processorMode: ProcessorMode)
  extends Conditional with ReferencingInstruction {

  val pageMap = new TrieMap[Section, ARMReferencingInstructionOnPage]

  def encodeWordForDistance(destination: PointerType)(implicit page: Section): Int

  override def size()(implicit page: Section): Int = getOrElseCreateInstruction().size

  override def getOrElseCreateInstruction()(implicit page: Section): ARMReferencingInstructionOnPage = {
    val target = page.encodableLocation(page.getEncodableByCondition(label))
    pageMap.getOrElseUpdate(page, createOperation(page.encodableLocation(this), target, page, processorMode))
  }

  def createOperation(thisLocation: Int, targetLocation: Int, section: Section, processorMode: ProcessorMode):
  ARMReferencingInstructionOnPage =
    new ARMReferencingInstructionOnPage(thisLocation, targetLocation)(section, processorMode)

  override def toString = s"${super.toString()} $label"

  class ARMReferencingInstructionOnPage(thisLocation: Int, destinationLocation: Int)
                                       (implicit page: Section, processorMode: ProcessorMode)
    extends ReferencingInstructionOnPage(thisLocation, destinationLocation) {

    val branchSize = 4
    override val minimumSize: Int = branchSize
    override val maximumSize: Int = branchSize

    override def getSizeForDistance(forward: Boolean, distance: Int): Int = branchSize

    override def encodeForDistance(forward: Boolean, distance: Int)(implicit page: Section): List[Byte] =
      encodeWordForDistance(getPointerForDistance(forward, distance)).encodeLittleEndian

    def getPointerForDistance(forward: Boolean, distance: Int)(implicit page: Section): PointerType = {
      if (forward) {
        newPointer(distance - 4)
      } else {
        newPointer(-distance - 8)
      }
    }
  }

}