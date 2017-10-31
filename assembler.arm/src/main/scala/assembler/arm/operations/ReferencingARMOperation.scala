package assembler.arm.operations

import assembler.arm.{ArmOffsetFactory, ProcessorMode}
import assembler.arm.operands.ArmOffset
import assembler.arm.operands.Condition.Condition
import assembler.reference.SinglePassRelativeReference
import assembler._

abstract class ReferencingARMOperation(val label: Label, val opcode: String, override val target: Label,
                                                    val condition: Condition)
                                                   (implicit armOffsetFactory: ArmOffsetFactory)
  extends SinglePassRelativeReference[ArmOffset] with NamedConditional {

  val instructionSize = 4

  override def estimateSize: Estimate[Int] = Actual(instructionSize)

  override def sizeForDistance(offsetDirection: OffsetDirection, distance: Long): Int = instructionSize

  override def toString = s"$labelPrefix$mnemonicString $target"

  override implicit def offsetFactory: PositionalOffsetFactory[ArmOffset] = armOffsetFactory.positionalOffsetFactory
}
