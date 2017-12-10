package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operands.{Condition => _, _}
import assembler.arm.operations.{BranchImmediate, BranchRegister, NamedConditional}
import assembler.resource.{Encodable, RelativeReference}
import assembler.{Label, OffsetDirection, RelativeOffsetDirection}

abstract class BranchReference(label: Label, val opcode: String, target: Label, val condition: Condition)
  extends RelativeReference(target, label) with NamedConditional {

  override def sizeForDependencySize(distance: Int, offsetDirection: OffsetDirection): Int = 4

  override def possibleSizes: Set[Int] = Set(4)

  override def toString = s"$labelPrefix$mnemonicString $target"
}

class Branch(code: Byte, val opcode: String) {
  def apply(destination: RelativeA32Pointer, condition: Condition = Always)
           (implicit label: Label, processorMode: ProcessorMode):  BranchImmediate[RelativeA32Pointer] =
    Immediate(label, destination, condition)

  def apply(targetLabel: Label)(implicit label: Label): BranchReference =
    new BranchReference(label, opcode, targetLabel, Always) {
      override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Encodable =
        Immediate(label, RelativeA32Pointer(ArmRelativeOffset.positionalOffset(distance)(offsetDirection)), Always)
    }

  private def Immediate[AddressType<:RelativePointer](label: Label, destination: AddressType, condition: Condition = Always) =
    new BranchImmediate[AddressType](label, destination, condition, code, opcode)

  def apply(targetLabel: Label, condition: Condition)(implicit label: Label): RelativeReference =
    new BranchReference(label, opcode, targetLabel, condition) {
      override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Encodable =
        Immediate(label, RelativeA32Pointer(ArmRelativeOffset.positionalOffset(distance)(offsetDirection)), condition)
    }
}

class BranchExchange(registerCode: Byte, val opcode: String) {
  def apply(destination: GeneralRegister, condition: Condition = Always)(implicit label: Label): BranchRegister =
    Register(label, destination, condition)

  private def Register(label: Label, destination: GeneralRegister, condition: Condition = Always) =
    new BranchRegister(label, destination, condition, registerCode, opcode)
}

class BranchLinkExchange(immediateCode: Byte, registerCode: Byte, opcode: String) extends BranchExchange(registerCode, opcode) {
  def apply(destination: RelativeThumbPointer)(implicit label: Label, processorMode: ProcessorMode): BranchImmediate[RelativeThumbPointer] =
    Immediate(label, destination, Unpredictable)

  private def Immediate(label: Label, destination: RelativeThumbPointer, condition: Condition = Always) =
    new BranchImmediate(label, destination, condition, immediateCode, opcode)

  def apply(targetLabel: Label)(implicit label: Label): BranchReference =
    new BranchReference(label, opcode, targetLabel, Unpredictable) {
      override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Encodable =
        Immediate(label, RelativeThumbPointer(ArmRelativeOffset.positionalOffset(distance)(offsetDirection)), Unpredictable)
    }
}

object Branch extends Branch(0xA0.toByte, "b")

object BranchLink extends Branch(0xB0.toByte, "bl")

object BranchExchange extends BranchExchange(0x1.toByte, "bx")

object BranchLinkExchange extends BranchLinkExchange(0xA0.toByte, 0x3.toByte, "blx")

object BranchExchangeJazelle extends BranchExchange(0x2.toByte, "bxj")
