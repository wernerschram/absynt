package assembler.arm.operations

import assembler.{Resource, Encodable, Label}
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition.Condition
import assembler.sections.Section
import assembler.reference.ReferencingInstruction

abstract class ReferencingARMOperation(val label: Label, val opcode: String, override val target: Label,
                                                    val condition: Condition)
                                                   (implicit processorMode: ProcessorMode)
  extends ReferencingInstruction with NamedConditional {

  val instructionSize = 4
  override val minimumSize: Int = instructionSize
  override val maximumSize: Int = instructionSize

  def encodableForDistance(distance: Int)(implicit page: Section): Resource with Encodable

  override def sizeForDistance(distance: Int)(forward: Boolean)(implicit page: Section): Int = instructionSize

  override def encodableForDistance(distance: Int)(forward: Boolean)(implicit page: Section): Resource with Encodable = {
    if (forward) {
      encodableForDistance(distance + instructionSize)
    } else {
      encodableForDistance(-distance)
    }
  }

  override def toString = s"$labelPrefix$mnemonicString $target"
}
