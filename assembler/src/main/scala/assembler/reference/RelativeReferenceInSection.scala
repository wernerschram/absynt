package assembler.reference

import assembler._
import assembler.sections.Section

import scala.annotation.tailrec

class RelativeReferenceInSection[OffsetType<:Offset] (
  private val destination: Label, val label: Label,
  val initialEstimatedSize: Estimate[Int],
  val encodableForOffset: (OffsetType)=> Resource with Encodable,
  val sizeForDistance: (OffsetDirection, Long)=> Int,
  val intermediateInstructions: Seq[Resource],
  val offsetDirection: OffsetDirection
  )(implicit section: Section[OffsetType],
  positionalOffsetFactory: PositionalOffsetFactory[OffsetType]) extends Resource with Encodable {

  private lazy val (
    dependentReferences: Seq[RelativeReference[OffsetType]],
    independentResources: Seq[Resource]) =
  intermediateInstructions.partition {
    case _: RelativeReference[OffsetType] => true
    case _ => false
  }

  private lazy val dependentReferencesInSection: Seq[RelativeReferenceInSection[OffsetType]] =
    dependentReferences.map(_.toInSectionState(section))

  private lazy val independentEstimatedDistance: Estimate[Int] =
    independentResources.map(_.estimateSize).estimateSum

  private def estimatedDistance: Estimate[Int] =
    intermediateInstructions.map(_.estimateSize).estimateSum

  private lazy val actualOffset: OffsetType = independentEstimatedDistance match {
    case a: Actual[Int] =>
      val distance = dependentReferencesInSection.map { _.size }.sum + a.value
      positionalOffsetFactory.offset(sizeForDistance(offsetDirection, distance), offsetDirection, distance)
    case _ => throw new AssertionError()
  }

  private def currentEstimatedSize: Estimate[Int] = estimatedDistance.map(e => sizeForDistance(offsetDirection, e))

  private var _estimatedSize: Option[Int] = None

  def isEstimated: Boolean = _estimatedSize.isDefined

  private def predictedOffset(sizeAssumptions: Map[RelativeReferenceInSection[OffsetType], Int]) = {
    assert(currentEstimatedSize.isInstanceOf[Bounded[Int]])
    independentEstimatedDistance match {
      case a: Actual[Int] =>
        dependentReferencesInSection.map { (instruction) =>
          sizeAssumptions.getOrElse(instruction,
            instruction.estimateSize(currentEstimatedSize.asInstanceOf[Bounded[Int]].minimum, sizeAssumptions))
        }.sum + a.value
      case _ => throw new AssertionError()
    }
  }

  @tailrec
  private[RelativeReferenceInSection] final def estimateSize(assumption: Int, sizeAssumptions: Map[RelativeReferenceInSection[OffsetType], Int]): Int = {
    val newSize = sizeForDistance(offsetDirection, predictedOffset(sizeAssumptions + (this -> assumption)))
    if (newSize < assumption) estimateSize(newSize, sizeAssumptions) else newSize
  }

  override def estimateSize: Estimate[Int] = if (isEstimated) Actual(size) else initialEstimatedSize

  def size: Int = {
    if (_estimatedSize.isEmpty) {
      currentEstimatedSize match {
        case actual: Actual[Int] => _estimatedSize = Some(actual.value)
        case bounded: Bounded[Int] => _estimatedSize = Some(estimateSize(bounded.minimum, collection.immutable.HashMap()))
        case _ => throw new AssertionError()
      }
    }
    _estimatedSize.get
  }

  lazy val encodeByte: Seq[Byte] = encodableForOffset(actualOffset).encodeByte
}
