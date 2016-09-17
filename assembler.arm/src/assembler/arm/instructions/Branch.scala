package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.opcodes.BranchImmediate
import assembler.arm.opcodes.BranchRegister
import assembler.arm.operands.Condition._
import assembler.arm.operands.RelativePointer
import assembler.arm.operands.registers.GeneralRegister
import assembler.reference.BranchInstructionOnPage
import assembler.PageLocation
import assembler.MemoryPage
import assembler.LabelCondition

class Branch(code: Byte, val opcode: String) {
  private val Immediate = new BranchImmediate(code)(opcode)

  def apply(destination: RelativePointer, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Immediate(destination, condition)
  
//  class ARMBranchInstructionOnPage extends BranchInstructionOnPage with ReferencingARMInstructionOnPage

  class ARMBranchInstructionOnPage private[Branch](thisLocation: PageLocation, destinationLocation: PageLocation, condition: Condition)(implicit page: MemoryPage, processorMode: ProcessorMode)
      extends BranchInstructionOnPage(thisLocation, destinationLocation) with ReferencingARMInstructionOnPage {
    
    val branchSize = 4
    override val minimumSize = branchSize
    override val maximumSize = branchSize

    override def getSizeForDistance(forward: Boolean, distance: Int) = branchSize

    private def instructionForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage) = {
      if (forward) {
        apply(RelativePointer(distance - 4), condition)
      } else {
        apply(RelativePointer(-distance - branchSize - 4), condition)
      }
    }
    
    def encodeWordForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage) = 
      instructionForDistance(forward, distance).encodeWord()

    override lazy val encodeWord = encodeWordForDistance(forward, actualDistance)

    override def encodeForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage) = 
      instructionForDistance(forward, distance).encodeByte()
  }
   
  def apply(labelCondition: LabelCondition)(implicit processorMode: ProcessorMode) =
    new ReferencingARMInstruction[ARMBranchInstructionOnPage](
      (thisLocation, targetLocation, memoryPage, processorMode) =>  
        new ARMBranchInstructionOnPage(thisLocation, targetLocation, Always)(memoryPage, processorMode), 
        opcode, labelCondition)  

  def apply(labelCondition: LabelCondition, condition: Condition)(implicit processorMode: ProcessorMode) =
    new ReferencingARMInstruction[ARMBranchInstructionOnPage](
      (thisLocation, targetLocation, memoryPage, processorMode) =>  
        new ARMBranchInstructionOnPage(thisLocation, targetLocation, condition)(memoryPage, processorMode), 
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
    
  class ARMBranchInstructionOnPage private[BranchLinkExchange](thisLocation: PageLocation, destinationLocation: PageLocation)(implicit page: MemoryPage, processorMode: ProcessorMode)
      extends BranchInstructionOnPage(thisLocation, destinationLocation) with ReferencingARMInstructionOnPage  {
    
    val branchSize = 1
    override val minimumSize = branchSize
    override val maximumSize = branchSize

    override def getSizeForDistance(forward: Boolean, distance: Int) = branchSize

    private def instructionForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage) = {
      if (forward) {
        apply(RelativePointer(distance - 4))
      } else {
        apply(RelativePointer(-distance - branchSize - 8))
      }
    }
    
    def encodeWordForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage) = 
      instructionForDistance(forward, distance).encodeWord()

    override lazy val encodeWord = encodeWordForDistance(forward, actualDistance)

    override def encodeForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage) = 
      instructionForDistance(forward, distance).encodeByte()
  }    
  
  def apply(labelCondition: LabelCondition)(implicit processorMode: ProcessorMode) =
    new ReferencingARMInstruction[ARMBranchInstructionOnPage](
      (thisLocation, targetLocation, memoryPage, processorMode) =>  
        new ARMBranchInstructionOnPage(thisLocation, targetLocation)(memoryPage, processorMode), 
        opcode, labelCondition)  

}

object Branch extends Branch(0xA0.toByte, "b")
object BranchLink extends Branch(0xB0.toByte, "bl")
object BranchExchange extends BranchExchange(0x1.toByte, "bx")
object BranchLinkExchange extends BranchLinkExchange(0xA0.toByte, 0x3.toByte, "blx")
object BranchExchangeJazelle extends BranchExchange(0x2.toByte, "bxj")
