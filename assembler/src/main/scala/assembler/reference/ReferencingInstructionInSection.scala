package assembler.reference

import assembler.{Resource, Encodable, Label}
import assembler.sections.Section

class ReferencingInstructionInSection (
  private val destination: Label, val label: Label,
  override val minimumSize: Int, override val maximumSize: Int,
  val encodableForDistance: (Int)=> Resource with Encodable,
  val sizeForDistance: (Int)=> Int,
  val intermediateInstructions: Seq[Resource]
  )(implicit section: Section) extends Resource with Encodable {

  def encodeForDistance(distance: Int): Seq[Byte] =
    encodableForDistance(distance).encodeByte

  def toFinalState = encodableForDistance(actualDistance)

  private lazy val independentIntermediates: Seq[Resource with Encodable] = intermediateInstructions.collect {
    case e: Resource with Encodable => e
  }

  private lazy val dependentIntermediates = intermediateInstructions.collect {
    // TODO: only works as long as there is only FinalState and ReferencingInstruction
    case e: ReferencingInstruction => e.toOnPageState(section)
  }

  private lazy val independentDistance =
    independentIntermediates.map { instruction => instruction.size }.sum

  private def minimumDistance = independentDistance + dependentIntermediates.map(instruction =>
    if (instruction.isEstimated) instruction.size else instruction.minimumSize).sum

  private def maximumDistance = independentDistance + dependentIntermediates.map(instruction =>
    if (instruction.isEstimated) instruction.size else instruction.maximumSize).sum

  lazy val actualDistance: Int = independentDistance + dependentIntermediates.map { instruction => instruction.size }.sum

  def minimumEstimatedSize: Int = encodableForDistance(minimumDistance).size
  def maximumEstimatedSize: Int = encodableForDistance(maximumDistance).size

  private var _estimatedSize: Option[Int] = None
  def isEstimated: Boolean = _estimatedSize.isDefined

  private def predictedDistance(sizeAssumptions: Map[ReferencingInstructionInSection, Int]) = independentDistance +
    dependentIntermediates.map { instruction =>
      if (sizeAssumptions.contains(instruction))
        sizeAssumptions(instruction)
      else
        instruction.estimateSize(sizeAssumptions)
    }
    .sum

  def estimateSize(sizeAssumptions: Map[ReferencingInstructionInSection, Int]): Int = {
    var assumption: Option[Int] = None
    var newAssumption = minimumEstimatedSize
    while (assumption.isEmpty || assumption.get < newAssumption) {
      assumption = Some(newAssumption)
      newAssumption = sizeForDistance(predictedDistance(sizeAssumptions + (this -> assumption.get)))
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

  lazy val encodeByte: Seq[Byte] = encodeForDistance(actualDistance)
}
