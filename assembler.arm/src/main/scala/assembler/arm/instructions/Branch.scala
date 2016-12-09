package assembler.arm.instructions

import assembler.LabelCondition
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operands.RelativeA32Pointer
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operations.BranchImmediate
import assembler.arm.operations.BranchRegister
import assembler.memory.MemoryPage
import assembler.arm.operands.RelativeThumbPointer

class Branch(code: Byte, val opcode: String) {
  private def Immediate(destination: RelativeA32Pointer, condition: Condition = Always) =
    new BranchImmediate(destination, condition, code, opcode)

  def apply(destination: RelativeA32Pointer, condition: Condition = Always)(implicit processorMode: ProcessorMode): BranchImmediate =
    Immediate(destination, condition)

  def apply(labelCondition: LabelCondition)(implicit processorMode: ProcessorMode) =
    new ReferencingARMInstruction[RelativeA32Pointer](opcode, labelCondition, RelativeA32Pointer.apply) {
      override def encodeWordForDistance(destination: RelativeA32Pointer)(implicit page: MemoryPage): Int =
        Immediate(destination, Always).encodeWord()
    }

  def apply(labelCondition: LabelCondition, condition: Condition)(implicit processorMode: ProcessorMode) =
    new ReferencingARMInstruction[RelativeA32Pointer](opcode, labelCondition, RelativeA32Pointer.apply) {
      override def encodeWordForDistance(destination: RelativeA32Pointer)(implicit page: MemoryPage): Int =
        Immediate(destination, condition).encodeWord()
    }
}

class BranchExchange(registerCode: Byte, val opcode: String) {
  private def Register(destination: GeneralRegister, condition: Condition = Always) =
    new BranchRegister(destination, condition, registerCode, opcode)

  def apply(destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Register(destination, condition)
}

class BranchLinkExchange(immediateCode: Byte, registerCode: Byte, opcode: String) extends BranchExchange(registerCode, opcode) {
  private def Immediate(destination: RelativeThumbPointer, condition: Condition = Always) =
    new BranchImmediate(destination, condition, immediateCode, opcode)

  def apply(destination: RelativeThumbPointer)(implicit processorMode: ProcessorMode) =
    Immediate(destination, Unpredictable)

  def apply(labelCondition: LabelCondition)(implicit processorMode: ProcessorMode) =
    new ReferencingARMInstruction[RelativeThumbPointer](opcode, labelCondition, RelativeThumbPointer.apply) {
      override def encodeWordForDistance(destination: RelativeThumbPointer)(implicit page: MemoryPage): Int =
        Immediate(destination, Unpredictable).encodeWord()
    }
}

object Branch extends Branch(0xA0.toByte, "b")
object BranchLink extends Branch(0xB0.toByte, "bl")
object BranchExchange extends BranchExchange(0x1.toByte, "bx")
object BranchLinkExchange extends BranchLinkExchange(0xA0.toByte, 0x3.toByte, "blx")
object BranchExchangeJazelle extends BranchExchange(0x2.toByte, "bxj")
