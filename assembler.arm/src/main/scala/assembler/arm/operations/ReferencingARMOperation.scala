package assembler.arm.operations

import assembler._
import assembler.arm.operands.Condition.Condition
import assembler.reference.RelativeReference

abstract class ReferencingARMOperation(val label: Label, val opcode: String, override val target: Label,
                                                    val condition: Condition)
  extends RelativeReference with NamedConditional {

  override def sizeForDependencySize(distance: Int, offsetDirection: OffsetDirection): Int =
    encodeForDependencySize(distance, offsetDirection).size

  override def possibleSizes: Set[Int] = Set(4, 8, 12, 16)

  override def toString = s"$labelPrefix$mnemonicString $target"
}
