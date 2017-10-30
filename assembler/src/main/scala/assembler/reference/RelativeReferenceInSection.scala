package assembler.reference

import assembler._
import assembler.sections.Section

import scala.annotation.tailrec

class RelativeReferenceInSection[OffsetType<:Offset] private(
  val section: Section[OffsetType],
  val destination: Label,
  val label: Label,
  val initialEstimatedSize: Estimate[Int],
  encodableForOffset: (OffsetType)=> Resource with Encodable,
  sizeForDistance: (OffsetDirection, Long)=> Int,
  val intermediateInstructions: Seq[Resource],
  val offsetDirection: OffsetDirection
  )(positionalOffsetFactory: PositionalOffsetFactory[OffsetType]) extends Resource with Encodable {

  private lazy val (
    dependentReferencesInSection: Seq[RelativeReferenceInSection[OffsetType]],
    independentEstimatedDistance: Estimate[Int]) = {
    val (dependent: Seq[RelativeReference[OffsetType]], independent: Seq[Resource]) =
      intermediateInstructions.partition {
        case _: RelativeReference[OffsetType] => true
        case _ => false
      }
    (dependent.map(_.toInSectionState(section)), independent.map(_.estimateSize).estimateSum))
  }
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

  private def estimatedOffset(sizeAssumptions: Map[RelativeReferenceInSection[OffsetType], Int]) = {
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
    val newSize = sizeForDistance(offsetDirection, estimatedOffset(sizeAssumptions + (this -> assumption)))
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

object RelativeReferenceInSection {
  def apply[OffsetType<:Offset](section: Section[OffsetType], reference: RelativeReference[OffsetType],
    intermediateInstructions: Seq[Resource], offsetDirection: OffsetDirection):
  RelativeReferenceInSection[OffsetType] =
    new RelativeReferenceInSection(section, reference.target, reference.label, reference.estimateSize,
      reference.encodableForOffset, reference.sizeForDistance, intermediateInstructions, offsetDirection)(reference.offsetFactory)
}
