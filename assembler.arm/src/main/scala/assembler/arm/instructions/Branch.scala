package assembler.arm.instructions

import assembler.LabelCondition
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operands.RelativePointer
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operations.BranchImmediate
import assembler.memory.MemoryPage
import assembler.arm.operations.BranchRegister

class Branch(code: Byte, val opcode: String) {
  private def Immediate(destination: RelativePointer, condition: Condition = Always) =
    new BranchImmediate(destination, condition, code, opcode)

  def apply(destination: RelativePointer, condition: Condition = Always)(implicit processorMode: ProcessorMode): BranchImmediate =
    Immediate(destination, condition)

  def apply(labelCondition: LabelCondition)(implicit processorMode: ProcessorMode) =
    new ReferencingARMInstruction(opcode, labelCondition) {
      override def encodeWordForDistance(destination: RelativePointer)(implicit page: MemoryPage): Int =
        Immediate(destination, Always).encodeWord()
    }

  def apply(labelCondition: LabelCondition, condition: Condition)(implicit processorMode: ProcessorMode) =
    new ReferencingARMInstruction(opcode, labelCondition) {
      override def encodeWordForDistance(destination: RelativePointer)(implicit page: MemoryPage): Int =
        Immediate(destination, condition).encodeWord()
    }
}

class BranchExchange(registerCode: Byte, val opcode: String) {
  // TODO HBit
//  private val Register = new BranchRegisterOperation(registerCode)(opcode)
  private def Register(destination: GeneralRegister, condition: Condition = Always) =
    new BranchRegister(destination, condition, registerCode, opcode)

  def apply(destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Register(destination, condition)
}

class BranchLinkExchange(immediateCode: Byte, registerCode: Byte, opcode: String) extends BranchExchange(registerCode, opcode) {
  // TODO HBit
  //  private val Immediate = new BranchImmediateOperation(immediateCode)(opcode)
  private def Immediate(destination: RelativePointer, condition: Condition = Always) =
    new BranchImmediate(destination, condition, immediateCode, opcode)

  def apply(destination: RelativePointer)(implicit processorMode: ProcessorMode) =
    Immediate(destination, Unpredictable)

  def apply(labelCondition: LabelCondition)(implicit processorMode: ProcessorMode) =
    new ReferencingARMInstruction(opcode, labelCondition) {
      override def encodeWordForDistance(destination: RelativePointer)(implicit page: MemoryPage): Int =
        Immediate(destination, Unpredictable).encodeWord()
    }
}

object Branch extends Branch(0xA0.toByte, "b")
object BranchLink extends Branch(0xB0.toByte, "bl")
object BranchExchange extends BranchExchange(0x1.toByte, "bx")
object BranchLinkExchange extends BranchLinkExchange(0xA0.toByte, 0x3.toByte, "blx")
object BranchExchangeJazelle extends BranchExchange(0x2.toByte, "bxj")
