package assembler.arm.operations

import assembler._
import assembler.arm.operands.Condition.Condition
import assembler.resource.RelativeReference

abstract class ReferencingARMOperation(label: Label, val opcode: String, target: Label, val condition: Condition)
  extends RelativeReference(target, label) with NamedConditional {

  override def sizeForDependencySize(distance: Int, offsetDirection: OffsetDirection): Int =
    encodableForDependencySize(distance, offsetDirection).size

  override def possibleSizes: Set[Int] = Set(4, 8, 12, 16)

  override def toString = s"$labelPrefix$mnemonicString $target"
}
