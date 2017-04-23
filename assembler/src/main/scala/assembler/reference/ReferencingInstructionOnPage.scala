package assembler.reference

import assembler.Encodable
import assembler.sections.Section
import assembler.Label

class ReferencingInstructionOnPage (
  private val thisOperation: ReferencingInstruction,
  private val destination: Label)(implicit section: Section) {

  val forward: Boolean = section.isForwardReference(thisOperation)

  private val intermediateInstructions = section.intermediateEncodables(thisOperation)

  private lazy val independentIntermediates: Seq[Encodable] = intermediateInstructions.filter {
    case _: ReferencingInstruction => false
    case _ => true
  }

  private lazy val dependentIntermediates = intermediateInstructions.filter {
    case _: ReferencingInstruction => true
    case _ => false
  }.map { i => i.asInstanceOf[ReferencingInstruction] }

  private lazy val independentDistance =
    independentIntermediates.map { instruction => instruction.size }.sum

  private def minimumDistance = independentDistance + dependentIntermediates.map(instruction =>
    if (instruction.isEstimated) instruction.size else instruction.minimumSize).sum

  private def maximumDistance = independentDistance + dependentIntermediates.map(instruction =>
    if (instruction.isEstimated) instruction.size else instruction.maximumSize).sum

  lazy val actualDistance: Int = independentDistance + dependentIntermediates.map { instruction => instruction.size }.sum

  def minimumEstimatedSize: Int = thisOperation.getSizeForDistance(forward, minimumDistance)
  def maximumEstimatedSize: Int = thisOperation.getSizeForDistance(forward, maximumDistance)

  private var _estimatedSize: Option[Int] = None
  def isEstimated: Boolean = _estimatedSize.isDefined

  private def predictedDistance(sizeAssumptions: Map[ReferencingInstructionOnPage, Int]) = independentDistance +
    dependentIntermediates.map { instruction =>
      if (sizeAssumptions.contains(instruction.getOrElseCreateInstruction()))
        sizeAssumptions(instruction.getOrElseCreateInstruction())
      else
        instruction.estimatedSize(sizeAssumptions)
    }
    .sum

  def estimateSize(sizeAssumptions: Map[ReferencingInstructionOnPage, Int]): Int = {
    var assumption: Option[Int] = None
    var newAssumption = minimumEstimatedSize
    while (assumption.isEmpty || assumption.get < newAssumption) {
      assumption = Some(newAssumption)
      newAssumption = thisOperation.getSizeForDistance(forward, predictedDistance(sizeAssumptions + (this -> assumption.get)))
    }
    newAssumption
  }

  def size: Int = {
    if (_estimatedSize.isEmpty) {
      if (minimumEstimatedSize == maximumEstimatedSize) {
        _estimatedSize = Some(minimumEstimatedSize)
      } else {
        _estimatedSize = Some(estimateSize(collection.immutable.HashMap()))
      }
    }
    _estimatedSize.get
  }

  lazy val encodeByte: List[Byte] = thisOperation.encodeForDistance(forward, actualDistance)
}
