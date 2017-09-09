package assembler.reference

import assembler.{Resource, Encodable, Label}
import assembler.sections.Section

import scala.collection.concurrent.TrieMap

trait ReferencingInstruction
    extends Resource {
  def target: Label

  def minimumSize: Int
  def maximumSize: Int

  def encodableForDistance(forward: Boolean, distance: Int)(implicit page: Section): Resource with Encodable

  def getSizeForDistance(forward: Boolean, distance: Int)(implicit page: Section): Int =
    encodableForDistance(forward, distance).size

  def encodeForDistance(forward: Boolean, distance: Int)(implicit page: Section): Seq[Byte] =
    encodableForDistance(forward, distance).encodeByte

  private val pageMap = new TrieMap[Section, ReferencingInstructionInSection]

  def getFinalState()(implicit page: Section): ReferencingInstructionInSection =
    pageMap.getOrElseUpdate(page, new ReferencingInstructionInSection(this, target, label)(page))

  final private[reference] def isEstimated()(implicit page: Section): Boolean = getFinalState.isEstimated

  final private[reference] def estimatedSize(sizeAssumptions: Map[ReferencingInstruction, Int])(implicit page: Section): Int =
    getFinalState.estimateSize(sizeAssumptions)
}
