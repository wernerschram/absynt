package assembler.arm.operations

import assembler.{Encodable, Label}
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition.Condition
import assembler.sections.Section
import assembler.reference.ReferencingInstruction

abstract class ReferencingARMOperation[PointerType](val label: Label, val opcode: String, override val target: Label,
                                                    val condition: Condition, newPointer: (Int, Section) => PointerType)
                                                   (implicit processorMode: ProcessorMode)
  extends Conditional with ReferencingInstruction {

  val branchSize = 4
  override val minimumSize: Int = branchSize
  override val maximumSize: Int = branchSize

  def encodableForDistance(destination: PointerType)(implicit page: Section): Encodable

  override def getSizeForDistance(forward: Boolean, distance: Int)(implicit page: Section): Int = branchSize

  def encodableForDistance(forward: Boolean, distance: Int)(implicit page: Section): Encodable = {
    if (forward) {
      encodableForDistance(newPointer(distance - 4, page))
    } else {
      encodableForDistance(newPointer(-distance - 8, page))
    }
  }

  override def toString = s"${super.toString()} $target"

}