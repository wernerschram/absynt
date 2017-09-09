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

  def sizeForDistance(forward: Boolean, distance: Int)(implicit page: Section): Int =
    encodableForDistance(forward, distance).size

  private val pageMap = new TrieMap[Section, ReferencingInstructionInSection]

  def toOnPageState()(implicit page: Section): ReferencingInstructionInSection =
    pageMap.getOrElseUpdate(page, new ReferencingInstructionInSection(this, target, label, minimumSize, maximumSize,
      encodableForDistance, sizeForDistance)(page))

}

