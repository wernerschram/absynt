package assembler.arm.instructions

import assembler.LabelCondition
import assembler.ListExtensions._
import assembler.arm.ProcessorMode
import assembler.arm.opcodes.BranchImmediate
import assembler.arm.opcodes.BranchRegister
import assembler.arm.operands.Condition._
import assembler.arm.operands.RelativePointer
import assembler.arm.operands.registers.GeneralRegister
import assembler.memory.MemoryPage
import assembler.reference.BranchInstructionOnPage

class ARMBranchInstructionOnPage(branch: BranchImmediate, thisLocation: Int, destinationLocation: Int, condition: Condition)(implicit page: MemoryPage, processorMode: ProcessorMode)
    extends BranchInstructionOnPage(thisLocation, destinationLocation) with ReferencingARMInstructionOnPage {

  val branchSize = 4
  override val minimumSize = branchSize
  override val maximumSize = branchSize

  override def getSizeForDistance(forward: Boolean, distance: Int) = branchSize

  def encodeWordForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage) =
    branch.apply(getPointerForDistance(forward, distance), condition).encodeWord()

  override lazy val encodeWord = encodeWordForDistance(forward, actualDistance)

  override def encodeForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage) =
    encodeWordForDistance(forward, distance).encodeLittleEndian
}

class Branch(code: Byte, val opcode: String) {
  private val Immediate = new BranchImmediate(code)(opcode)

  def apply(destination: RelativePointer, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Immediate(destination, condition)

  def apply(labelCondition: LabelCondition)(implicit processorMode: ProcessorMode) =
    new ReferencingARMInstruction[ARMBranchInstructionOnPage](
      (thisLocation, targetLocation, memoryPage, processorMode) =>
        new ARMBranchInstructionOnPage(Immediate, thisLocation, targetLocation, Always)(memoryPage, processorMode),
      opcode, labelCondition)

  def apply(labelCondition: LabelCondition, condition: Condition)(implicit processorMode: ProcessorMode) =
    new ReferencingARMInstruction[ARMBranchInstructionOnPage](
      (thisLocation, targetLocation, memoryPage, processorMode) =>
        new ARMBranchInstructionOnPage(Immediate, thisLocation, targetLocation, condition)(memoryPage, processorMode),
      opcode, labelCondition)
}

class BranchExchange(registerCode: Byte, val opcode: String) {
  // TODO HBit
  private val Register = new BranchRegister(registerCode)(opcode)

  def apply(destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Register(destination, condition)
}

class BranchLinkExchange(immediateCode: Byte, registerCode: Byte, opcode: String) extends BranchExchange(registerCode, opcode) {
  // TODO HBit
  private val Immediate = new BranchImmediate(immediateCode)(opcode)

  def apply(destination: RelativePointer)(implicit processorMode: ProcessorMode) =
    Immediate(destination, Unpredictable)

  def apply(labelCondition: LabelCondition)(implicit processorMode: ProcessorMode) =
    new ReferencingARMInstruction[ARMBranchInstructionOnPage](
      (thisLocation, targetLocation, memoryPage, processorMode) =>
        new ARMBranchInstructionOnPage(Immediate, thisLocation, targetLocation, Unpredictable)(memoryPage, processorMode),
      opcode, labelCondition)
}

object Branch extends Branch(0xA0.toByte, "b")
object BranchLink extends Branch(0xB0.toByte, "bl")
object BranchExchange extends BranchExchange(0x1.toByte, "bx")
object BranchLinkExchange extends BranchLinkExchange(0xA0.toByte, 0x3.toByte, "blx")
object BranchExchangeJazelle extends BranchExchange(0x2.toByte, "bxj")
