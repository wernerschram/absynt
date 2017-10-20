package assembler.reference

import assembler._
import assembler.sections.Section

class RelativeReferenceInSection[OffsetType<:Offset] (
  private val destination: Label, val label: Label,
  override val minimumSize: Int, override val maximumSize: Int,
  val encodableForOffset: (OffsetType)=> Resource with Encodable,
  val sizeForDistance: (OffsetDirection, Long)=> Int,
  val intermediateInstructions: Seq[Resource],
  val offsetDirection: OffsetDirection
  )(implicit section: Section[OffsetType],
  positionalOffsetFactory: PositionalOffsetFactory[OffsetType]) extends Resource with Encodable {

  def encodeForDistance(offset: OffsetType): Seq[Byte] =
    encodableForOffset(offset).encodeByte

  def toFinalState = encodableForOffset(actualOffset)

  private lazy val (
    dependentReferences: Seq[RelativeReference[OffsetType]],
    dependentResources: Seq[Resource]) =
  intermediateInstructions.partition {
     case _: RelativeReference[OffsetType] => true
     case _ => false
  }

  private lazy val dependentReferencesInSection = dependentReferences.map{ _.toInSectionState(section) }

  private lazy val independentMinimumDistance: Long = dependentResources.map { _.minimumSize }.sum
  private lazy val independentMaximumDistance: Long = dependentResources.map { _.maximumSize }.sum

  private def minimumDistance: Long =
    dependentReferencesInSection.map(instruction =>
      if (instruction.isEstimated) instruction.size else instruction.minimumSize).sum +
      independentMinimumDistance

  private def maximumDistance: Long =
    dependentReferencesInSection.map(instruction =>
      if (instruction.isEstimated) instruction.size else instruction.maximumSize).sum +
      independentMaximumDistance

  lazy val actualOffset: OffsetType = {
    assert(independentMinimumDistance == independentMaximumDistance)
    val distance = dependentReferencesInSection.map { _.size }.sum + independentMinimumDistance
    positionalOffsetFactory.offset(sizeForDistance(offsetDirection, distance), offsetDirection, distance)
  }

  def minimumEstimatedSize: Int = sizeForDistance(offsetDirection, minimumDistance)
  def maximumEstimatedSize: Int = sizeForDistance(offsetDirection, maximumDistance)

  private var _estimatedSize: Option[Int] = None

  def isEstimated: Boolean = _estimatedSize.isDefined

  private def predictedOffset(sizeAssumptions: Map[RelativeReferenceInSection[OffsetType], Int]) = {
    assert(independentMinimumDistance == independentMaximumDistance)
    dependentReferencesInSection.map { instruction =>
        if (sizeAssumptions.contains(instruction))
          sizeAssumptions(instruction)
        else
          instruction.estimateSize(sizeAssumptions)
      }.sum + independentMinimumDistance
  }

  def estimateSize(sizeAssumptions: Map[RelativeReferenceInSection[OffsetType], Int]): Int = {
    var assumption: Option[Int] = None
    var newAssumption = minimumEstimatedSize
    while (assumption.isEmpty || assumption.get < newAssumption) {
      assumption = Some(newAssumption)
      newAssumption = sizeForDistance(offsetDirection, predictedOffset(sizeAssumptions + (this -> assumption.get)))
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

  lazy val encodeByte: Seq[Byte] = encodeForDistance(actualOffset)
}
