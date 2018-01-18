package assembler.arm.instructions

import assembler.arm.operands.Condition._
import assembler.arm.operands.{ArmRelativeOffset, RelativeA32Pointer, RelativePointer, RelativeThumbPointer}
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operations.{BranchImmediate, BranchRegister, NamedConditional}
import assembler.resource.{UnlabeledEncodable, RelativeReference}
import assembler.{Label, OffsetDirection, RelativeOffsetDirection}

abstract class BranchReference(val opcode: String, target: Label, val condition: Condition)
  extends RelativeReference(target) with NamedConditional {

  override def sizeForDependencySize(distance: Int, offsetDirection: OffsetDirection): Int = 4

  override def possibleSizes: Set[Int] = Set(4)

  override def toString = s"$mnemonicString $target"
}

class Branch(code: Byte, val opcode: String) {
  def apply(destination: RelativeA32Pointer, condition: Condition = Always):  BranchImmediate[RelativeA32Pointer] =
    Immediate(destination, condition)

  def apply(targetLabel: Label): BranchReference =
    new BranchReference(opcode, targetLabel, Always) {
      override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): UnlabeledEncodable =
        Immediate(RelativeA32Pointer(ArmRelativeOffset.positionalOffset(distance)(offsetDirection)), Always)
    }

  private def Immediate[AddressType<:RelativePointer](destination: AddressType, condition: Condition = Always) =
    new BranchImmediate[AddressType](destination, condition, code, opcode)

  def apply(targetLabel: Label, condition: Condition)(): RelativeReference =
    new BranchReference(opcode, targetLabel, condition) {
      override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): UnlabeledEncodable =
        Immediate(RelativeA32Pointer(ArmRelativeOffset.positionalOffset(distance)(offsetDirection)), condition)
    }
}

class BranchExchange(registerCode: Byte, val opcode: String) {
  def apply(destination: GeneralRegister, condition: Condition = Always)(): BranchRegister =
    Register(destination, condition)

  private def Register(destination: GeneralRegister, condition: Condition = Always) =
    new BranchRegister(destination, condition, registerCode, opcode)
}

class BranchLinkExchange(immediateCode: Byte, registerCode: Byte, opcode: String) extends BranchExchange(registerCode, opcode) {
  def apply(destination: RelativeThumbPointer): BranchImmediate[RelativeThumbPointer] =
    Immediate(destination, Unpredictable)

  private def Immediate(destination: RelativeThumbPointer, condition: Condition = Always) =
    new BranchImmediate(destination, condition, immediateCode, opcode)

  def apply(targetLabel: Label): BranchReference =
    new BranchReference(opcode, targetLabel, Unpredictable) {
      override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): UnlabeledEncodable =
        Immediate(RelativeThumbPointer(ArmRelativeOffset.positionalOffset(distance)(offsetDirection)), Unpredictable)
    }
}

object Branch extends Branch(0xA0.toByte, "b")

object BranchLink extends Branch(0xB0.toByte, "bl")

object BranchExchange extends BranchExchange(0x1.toByte, "bx")

object BranchLinkExchange extends BranchLinkExchange(0xA0.toByte, 0x3.toByte, "blx")

object BranchExchangeJazelle extends BranchExchange(0x2.toByte, "bxj")
