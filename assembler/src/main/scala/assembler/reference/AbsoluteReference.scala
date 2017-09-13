package assembler.reference

import assembler.sections.Section
import assembler.{Encodable, Label, Resource}

import scala.collection.concurrent.TrieMap

trait AbsoluteReference
    extends Resource {
  def target: Label

  def encodableForPosition(position: Int): Resource with Encodable

  def toInSectionState(section: Section) = {
    val newMinimum = section.content.map(instruction => instruction.minimumSize).sum
    val newMaximum = section.content.map(instruction => instruction.maximumSize).sum
    if (newMinimum == newMaximum)
      encodableForPosition(newMinimum)
    else
      new AbsoluteReference {
        override def encodableForPosition(position: Int): Resource with Encodable =
          AbsoluteReference.this.encodableForPosition(position)

        override def target: Label = AbsoluteReference.this.target

        override def label: Label = AbsoluteReference.this.label

        override def minimumSize: Int = encodableForPosition(newMinimum).size

        override def maximumSize: Int = encodableForPosition(newMaximum).size
      }
  }
}

