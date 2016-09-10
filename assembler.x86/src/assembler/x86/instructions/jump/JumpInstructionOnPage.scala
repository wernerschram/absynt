package assembler.x86.instructions.jump

import assembler.Encodable
import assembler.MemoryPage
import assembler.PageLocation
import assembler.ListExtensions._
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Instruction
import assembler.x86.instructions.ReferencingX86Instruction
import assembler.x86.operands.memoryaccess.NearPointer
import assembler.x86.ReferencingInstructionOnPage

abstract class JumpInstructionOnPage(
  private val thisLocation: PageLocation,
  private val destinationLocation: PageLocation)(implicit page: MemoryPage, processorMode: ProcessorMode)
    extends ReferencingInstructionOnPage() {
    
  def minimumSize: Int
  def maximumSize: Int
  
  def getSizeForDistance(forward: Boolean, distance: Int): Int

  def encodeForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage, processorMode: ProcessorMode): List[Byte]

  
  private val forward = (thisLocation < destinationLocation)

  private val intermediateInstructions = page.slice(thisLocation, destinationLocation)
  
  private lazy val independentIntermediates: List[Encodable] = intermediateInstructions.filter {
    case instruction: ReferencingX86Instruction[_] => false
    case _ => true
  }
  
  private lazy val dependentIntermediates = intermediateInstructions.filter {
    case instruction: ReferencingX86Instruction[_] => true
    case _ => false
  }.map{i => i.asInstanceOf[ReferencingX86Instruction[ReferencingInstructionOnPage]]}
  
  private lazy val independentDistance =
    independentIntermediates.map { instruction => instruction.size }.sum

  private def minimumDistance = independentDistance + dependentIntermediates.map(instruction => instruction.sizeIsKnown match {
    case true => instruction.size
    case false => minimumSize
  }).sum

  private def maximumDistance = independentDistance + dependentIntermediates.map(instruction => instruction.sizeIsKnown match {
    case true => instruction.size
    case false => maximumSize
  }).sum

  private lazy val actualDistance = independentDistance + dependentIntermediates.map { instruction => instruction.size }.sum

  override def minimumEstimatedSize: Int = getSizeForDistance(forward, minimumDistance)
  override def maximumEstimatedSize: Int = getSizeForDistance(forward, maximumDistance)

  // TODO: combine these two
  private var isEstimated = false

  override def sizeIsKnown: Boolean = isEstimated

  private def predictedDistance(sizeAssumptions: Map[ReferencingInstructionOnPage, Int]) = independentDistance +
    dependentIntermediates.map(instruction => sizeAssumptions.contains(instruction.getOrElseCreateInstruction()) match {
      case false => instruction.estimatedSize(sizeAssumptions)
      case true => sizeAssumptions.get(instruction.getOrElseCreateInstruction()).get
    }).sum

  override def estimatedSize(sizeAssumptions: Map[ReferencingInstructionOnPage, Int]) = {
    var assumption: Option[Int] = None
    var newAssumption = minimumEstimatedSize
    while (assumption.exists { value => value < newAssumption }) { {
        assumption = Some(newAssumption)
        newAssumption = getSizeForDistance(forward, predictedDistance(sizeAssumptions + (this -> assumption.get)))
      }
    }
    newAssumption
  }

  lazy val size: Int = if (minimumEstimatedSize == maximumEstimatedSize) {
    minimumEstimatedSize
  } else {
    val estimation = estimatedSize(collection.immutable.HashMap())
    isEstimated = true
    estimation
  }

  override lazy val encodeByte = encodeForDistance(forward, actualDistance) 
}