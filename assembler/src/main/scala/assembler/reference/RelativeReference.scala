package assembler.reference

import assembler.{Encodable, Label, Resource}
import assembler.sections.Section

import scala.collection.concurrent.TrieMap

trait RelativeReference
    extends Resource {
  def target: Label

  def encodableForDistance(distance: Int)(forward: Boolean): Resource with Encodable

  def sizeForDistance(distance: Int)(forward: Boolean): Int =
    encodableForDistance(distance)(forward).size

  private val sectionMap = new TrieMap[Section, RelativeReferenceInSection]

  def toInSectionState(section: Section): RelativeReferenceInSection = {
    val forward = section.isForwardReference(this)

    sectionMap.getOrElseUpdate(section, new RelativeReferenceInSection(target, label, minimumSize, maximumSize,
      encodableForDistance(_)(forward), sizeForDistance(_)(forward),
      section.intermediateEncodables(this))(section))
  }
}

