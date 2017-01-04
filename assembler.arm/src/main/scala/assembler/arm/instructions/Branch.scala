package assembler.arm.instructions

import assembler.Label
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operands.{RelativeA32Pointer, RelativeThumbPointer}
import assembler.arm.operations.{BranchImmediate, BranchRegister, ReferencingARMOperation}
import assembler.sections.Section

class Branch(code: Byte, val opcode: String) {
  def apply(destination: RelativeA32Pointer, condition: Condition = Always)(implicit processorMode: ProcessorMode): BranchImmediate =
    Immediate(destination, condition)

  def apply(label: Label)(implicit processorMode: ProcessorMode) =
    new ReferencingARMOperation[RelativeA32Pointer](opcode, label, Always, RelativeA32Pointer.apply) {
      override def encodeWordForDistance(destination: RelativeA32Pointer)(implicit page: Section): Int =
        Immediate(destination, Always).encodeWord()
    }

  private def Immediate(destination: RelativeA32Pointer, condition: Condition = Always) =
    new BranchImmediate(destination, condition, code, opcode)

  def apply(label: Label, condition: Condition)(implicit processorMode: ProcessorMode) =
    new ReferencingARMOperation[RelativeA32Pointer](opcode, label, condition, RelativeA32Pointer.apply) {
      override def encodeWordForDistance(destination: RelativeA32Pointer)(implicit page: Section): Int =
        Immediate(destination, condition).encodeWord()
    }
}

class BranchExchange(registerCode: Byte, val opcode: String) {
  def apply(destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Register(destination, condition)

  private def Register(destination: GeneralRegister, condition: Condition = Always) =
    new BranchRegister(destination, condition, registerCode, opcode)
}

class BranchLinkExchange(immediateCode: Byte, registerCode: Byte, opcode: String) extends BranchExchange(registerCode, opcode) {
  def apply(destination: RelativeThumbPointer)(implicit processorMode: ProcessorMode) =
    Immediate(destination, Unpredictable)

  private def Immediate(destination: RelativeThumbPointer, condition: Condition = Always) =
    new BranchImmediate(destination, condition, immediateCode, opcode)

  def apply(label: Label)(implicit processorMode: ProcessorMode) =
    new ReferencingARMOperation[RelativeThumbPointer](opcode, label, Unpredictable, RelativeThumbPointer.apply) {
      override def encodeWordForDistance(destination: RelativeThumbPointer)(implicit page: Section): Int =
        Immediate(destination, Unpredictable).encodeWord()
    }
}

object Branch extends Branch(0xA0.toByte, "b")

object BranchLink extends Branch(0xB0.toByte, "bl")

object BranchExchange extends BranchExchange(0x1.toByte, "bx")

object BranchLinkExchange extends BranchLinkExchange(0xA0.toByte, 0x3.toByte, "blx")

object BranchExchangeJazelle extends BranchExchange(0x2.toByte, "bxj")
