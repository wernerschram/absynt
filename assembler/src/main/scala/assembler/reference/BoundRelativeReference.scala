package assembler.reference

import assembler._
import assembler.sections.Section

import scala.annotation.tailrec

sealed class BoundRelativeReference[OffsetType<:Offset] private(
  val section: Section[OffsetType],
  val reference: SinglePassRelativeReference[OffsetType],
  val initialEstimatedSize: Estimate[Int]
  )(positionalOffsetFactory: PositionalOffsetFactory[OffsetType]) extends Resource with Encodable {

  val destination: Label = reference.target

  val label: Label = reference.label

  def intermediateInstructions: Seq[Resource] = section.intermediateEncodables(reference)

  def offsetDirection: OffsetDirection = section.offsetDirection(reference)

  def encodableForOffset(offset: OffsetType): Resource with Encodable = reference.encodableForOffset(offset)

  def sizeForDistance(offsetDirection: OffsetDirection, distance: Long): Int = reference.sizeForDistance(offsetDirection, distance)

  private lazy val (
    dependentReferencesInSection: Seq[SinglePassRelativeReference[OffsetType]],
    independentEstimatedDistance: Estimate[Int]) = {
    val (dependent: Seq[SinglePassRelativeReference[OffsetType]], independent: Seq[Resource]) =
      intermediateInstructions.partition {
        case _: SinglePassRelativeReference[OffsetType] => true
        case _ => false
      }
    (dependent, independent.map(_.estimateSize).estimateSum)
  }
  private def estimatedDistance: Estimate[Int] =
    intermediateInstructions.map(_.estimateSize).estimateSum

  private lazy val actualOffset: OffsetType = independentEstimatedDistance match {
    case a: Actual[Int] =>
      val distance = dependentReferencesInSection.map { _.size(section) }.sum + a.value
      positionalOffsetFactory.offset(sizeForDistance(offsetDirection, distance), offsetDirection, distance)
    case _ => throw new AssertionError()
  }

  private def currentEstimatedSize: Estimate[Int] = estimatedDistance.map(e => sizeForDistance(offsetDirection, e))

  private var _estimatedSize: Option[Int] = None

  def isEstimated: Boolean = _estimatedSize.isDefined

  private def estimatedOffset(sizeAssumptions: Map[SinglePassRelativeReference[OffsetType], Int]) = {
    assert(currentEstimatedSize.isInstanceOf[Bounded[Int]])
    independentEstimatedDistance match {
      case a: Actual[Int] =>
        dependentReferencesInSection.map { (instruction) =>
          sizeAssumptions.getOrElse(instruction,
            instruction.estimateSize(currentEstimatedSize.asInstanceOf[Bounded[Int]].minimum, sizeAssumptions)(section))
        }.sum + a.value
      case _ => throw new AssertionError()
    }
  }

  @tailrec
  final def estimateSize(assumption: Int, sizeAssumptions: Map[SinglePassRelativeReference[OffsetType], Int]): Int = {
    val newSize = sizeForDistance(offsetDirection, estimatedOffset(sizeAssumptions + (this.reference -> assumption)))
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

object BoundRelativeReference {
  def apply[OffsetType<:Offset](section: Section[OffsetType], reference: SinglePassRelativeReference[OffsetType]):
  BoundRelativeReference[OffsetType] =
    new BoundRelativeReference(section, reference, reference.estimateSize)(reference.offsetFactory)
}
