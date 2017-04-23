package assembler.arm.operations

import assembler.{Encodable, Label}
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition.Condition
import assembler.sections.Section
import assembler.reference.{ReferencingInstruction, ReferencingInstructionOnPage}

import scala.collection.concurrent.TrieMap

abstract class ReferencingARMOperation[PointerType](val label: Label, val opcode: String, override val target: Label,
                                                    val condition: Condition, newPointer: (Int, Section) => PointerType)
                                                   (implicit processorMode: ProcessorMode)
  extends Conditional with ReferencingInstruction {

  val branchSize = 4
  override val minimumSize: Int = branchSize
  override val maximumSize: Int = branchSize

  val pageMap = new TrieMap[Section, ARMReferencingInstructionOnPage]

  def encodableForDistance(destination: PointerType)(implicit page: Section): Encodable

  override def size()(implicit page: Section): Int = getOrElseCreateInstruction().size

  override def getOrElseCreateInstruction()(implicit page: Section): ARMReferencingInstructionOnPage = {
    pageMap.getOrElseUpdate(page, createOperation(this, target, page, processorMode))
  }

  def createOperation(thisOperation: ReferencingInstruction, destination: Label, section: Section, processorMode: ProcessorMode):
  ARMReferencingInstructionOnPage =
    new ARMReferencingInstructionOnPage(thisOperation, destination)(section, processorMode)

  override def toString = s"${super.toString()} $target"

  class ARMReferencingInstructionOnPage(thisOperation: ReferencingInstruction, destination: Label)
                                       (implicit page: Section, processorMode: ProcessorMode)
    extends ReferencingInstructionOnPage(thisOperation, destination) {

    override def getSizeForDistance(forward: Boolean, distance: Int): Int = branchSize

    override def encodeForDistance(forward: Boolean, distance: Int)(implicit page: Section): List[Byte] =
      ReferencingARMOperation.this.encodableForDistance(getPointerForDistance(forward, distance)).encodeByte

    def getPointerForDistance(forward: Boolean, distance: Int)(implicit page: Section): PointerType = {
      if (forward) {
        newPointer(distance - 4, page)
      } else {
        newPointer(-distance - 8, page)
      }
    }
  }

}