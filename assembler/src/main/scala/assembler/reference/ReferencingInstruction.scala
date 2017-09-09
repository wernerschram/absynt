package assembler.reference

import assembler.{Resource, Encodable, Label}
import assembler.sections.Section

import scala.collection.concurrent.TrieMap

trait ReferencingInstruction
    extends Resource {
  def target: Label

  def minimumSize: Int
  def maximumSize: Int

  def encodableForDistance(distance: Int)(forward: Boolean)(implicit page: Section): Resource with Encodable

  def sizeForDistance(distance: Int)(forward: Boolean)(implicit page: Section): Int =
    encodableForDistance(distance)(forward).size

  private val pageMap = new TrieMap[Section, ReferencingInstructionInSection]

  def toOnPageState()(section: Section): ReferencingInstructionInSection = {
    val forward = section.isForwardReference(this)

    pageMap.getOrElseUpdate(section, new ReferencingInstructionInSection(target, label, minimumSize, maximumSize,
      encodableForDistance(_)(forward)(section), sizeForDistance(_)(forward)(section),
      section.intermediateEncodables(this))(section))
  }
}

