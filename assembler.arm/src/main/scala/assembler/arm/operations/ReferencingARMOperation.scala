package assembler.arm.operations

import assembler._
import assembler.arm.ArmOffsetFactory
import assembler.arm.operands.ArmOffset
import assembler.arm.operands.Condition.Condition
import assembler.reference.SinglePassRelativeReference

abstract class ReferencingARMOperation(val label: Label, val opcode: String, override val target: Label,
                                                    val condition: Condition)
                                                   (implicit val offsetFactory: ArmOffsetFactory)
  extends SinglePassRelativeReference[ArmOffset] with NamedConditional {

  //FIXME: this is not correct for add.forRelativeLabel.
 val instructionSize = 4

  override def estimateSize: Estimate[Int] = Actual(instructionSize)

  override def sizeForDistance(distance: Int, offsetDirection: OffsetDirection): Int = instructionSize

  override def possibleSizes: List[Int] = instructionSize :: Nil

  override def toString = s"$labelPrefix$mnemonicString $target"
}
