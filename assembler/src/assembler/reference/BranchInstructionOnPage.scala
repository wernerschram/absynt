package assembler.reference

import assembler.Encodable
import assembler.memory.MemoryPage

abstract class BranchInstructionOnPage(
  private val thisLocation: Int,
  private val destinationLocation: Int)(implicit page: MemoryPage)
    extends ReferencingInstructionOnPage() {

  def minimumSize: Int
  def maximumSize: Int

  def getSizeForDistance(forward: Boolean, distance: Int): Int

  def encodeForDistance(forward: Boolean, distance: Int)(implicit page: MemoryPage): List[Byte]

  val forward = (thisLocation < destinationLocation)

  private val intermediateInstructions = page.intermediateEncodables(thisLocation, destinationLocation)

  private lazy val independentIntermediates: Seq[Encodable] = intermediateInstructions.filter {
    case instruction: ReferencingInstruction[_] => false
    case _ => true
  }

  private lazy val dependentIntermediates = intermediateInstructions.filter {
    case instruction: ReferencingInstruction[_] => true
    case _ => false
  }.map { i => i.asInstanceOf[ReferencingInstruction[ReferencingInstructionOnPage]] }

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

  lazy val actualDistance = independentDistance + dependentIntermediates.map { instruction => instruction.size }.sum

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
    while (assumption.exists { value => value < newAssumption }) {
      {
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