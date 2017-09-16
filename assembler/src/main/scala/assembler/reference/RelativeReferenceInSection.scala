package assembler.reference

import assembler.{Resource, Encodable, Label}
import assembler.sections.Section

class RelativeReferenceInSection (
  private val destination: Label, val label: Label,
  override val minimumSize: Int, override val maximumSize: Int,
  val encodableForDistance: (Int)=> Resource with Encodable,
  val sizeForDistance: (Int)=> Int,
  val intermediateInstructions: Seq[Resource]
  )(implicit section: Section) extends Resource with Encodable {

  def encodeForDistance(distance: Int): Seq[Byte] =
    encodableForDistance(distance).encodeByte

  def toFinalState = encodableForDistance(actualDistance)

  private lazy val (
    dependentReferences: Seq[RelativeReference],
    dependentResources: Seq[Resource]) =
  intermediateInstructions.partition {
     case _: RelativeReference => true
     case _ => false
  }

  private lazy val dependentReferencesInSection = dependentReferences.map{ _.toOnPageState(section) }

  private lazy val independentMinimumDistance: Int = dependentResources.map { _.minimumSize }.sum
  private lazy val independentMaximumDistance: Int = dependentResources.map { _.maximumSize }.sum

  private def minimumDistance = independentMinimumDistance +
    dependentReferencesInSection.map(instruction =>
      if (instruction.isEstimated) instruction.size else instruction.minimumSize).sum

  private def maximumDistance = independentMaximumDistance +
    dependentReferencesInSection.map(instruction =>
      if (instruction.isEstimated) instruction.size else instruction.maximumSize).sum

  lazy val actualDistance: Int = {
    assert(independentMinimumDistance == independentMaximumDistance)
    independentMinimumDistance + dependentReferencesInSection.map { _.size }.sum
  }

  def minimumEstimatedSize: Int = encodableForDistance(minimumDistance).size
  def maximumEstimatedSize: Int = encodableForDistance(maximumDistance).size

  private var _estimatedSize: Option[Int] = None
  def isEstimated: Boolean = _estimatedSize.isDefined

  private def predictedDistance(sizeAssumptions: Map[RelativeReferenceInSection, Int]) = {
    assert(independentMinimumDistance == independentMaximumDistance)
    independentMinimumDistance +
      dependentReferencesInSection.map { instruction =>
        if (sizeAssumptions.contains(instruction))
          sizeAssumptions(instruction)
        else
          instruction.estimateSize(sizeAssumptions)
      }.sum
  }

  def estimateSize(sizeAssumptions: Map[RelativeReferenceInSection, Int]): Int = {
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
