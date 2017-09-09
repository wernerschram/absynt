package assembler.reference

import assembler.{Resource, Encodable, Label}
import assembler.sections.Section

class ReferencingInstructionOnPage (
  private val thisOperation: ReferencingInstruction,
  private val destination: Label)(implicit section: Section) {

  val forward: Boolean = section.isForwardReference(thisOperation)

  private val intermediateInstructions = section.intermediateEncodables(thisOperation)

  private lazy val independentIntermediates: Seq[Resource with Encodable] = intermediateInstructions.collect {
    case e: Resource with Encodable => e
  }

  private lazy val dependentIntermediates = intermediateInstructions.collect {
    // TODO: only works as long as there is only FinalState and ReferencingInstruction
    case e: ReferencingInstruction => e
  }

  private lazy val independentDistance =
    independentIntermediates.map { instruction => instruction.size }.sum

  private def minimumDistance = independentDistance + dependentIntermediates.map(instruction =>
    if (instruction.isEstimated) instruction.getFinalState().size else instruction.minimumSize).sum

  private def maximumDistance = independentDistance + dependentIntermediates.map(instruction =>
    if (instruction.isEstimated) instruction.getFinalState().size else instruction.maximumSize).sum

  lazy val actualDistance: Int = independentDistance + dependentIntermediates.map { instruction => instruction.getFinalState().size }.sum

  def minimumEstimatedSize: Int = thisOperation.getSizeForDistance(forward, minimumDistance)
  def maximumEstimatedSize: Int = thisOperation.getSizeForDistance(forward, maximumDistance)

  private var _estimatedSize: Option[Int] = None
  def isEstimated: Boolean = _estimatedSize.isDefined

  private def predictedDistance(sizeAssumptions: Map[ReferencingInstruction, Int]) = independentDistance +
    dependentIntermediates.map { instruction =>
      if (sizeAssumptions.contains(instruction))
        sizeAssumptions(instruction)
      else
        instruction.estimatedSize(sizeAssumptions)
    }
    .sum

  def estimateSize(sizeAssumptions: Map[ReferencingInstruction, Int]): Int = {
    var assumption: Option[Int] = None
    var newAssumption = minimumEstimatedSize
    while (assumption.isEmpty || assumption.get < newAssumption) {
      assumption = Some(newAssumption)
      newAssumption = thisOperation.getSizeForDistance(forward, predictedDistance(sizeAssumptions + (thisOperation -> assumption.get)))
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

  lazy val encodeByte: Seq[Byte] = thisOperation.encodeForDistance(forward, actualDistance)
}
