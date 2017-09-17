package assembler.reference

import assembler.sections.Section
import assembler.{Encodable, Label, Resource}

sealed trait AbsoluteReference
    extends Resource {
  def target: Label

  def encodableForPosition(position: Int): Resource with Encodable

  def toInSectionState(section: Section): Resource = {
    val newMinimum = section.baseAddress + section.precedingResources(target).map(instruction => instruction.minimumSize).sum
    val newMaximum = section.baseAddress + section.precedingResources(target).map(instruction => instruction.maximumSize).sum
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

trait CurrentSection {
  self: AbsoluteReference =>
}

trait OtherSection {
  self: AbsoluteReference =>
  val sectionName: String
}

object AbsoluteReference {
  def apply(targetLabel: Label, initialMinimumSize: Int, initialMaximumSize: Int, thisLabel: Label,
    encodableFactory: (Int)=>Resource with Encodable) =
    new AbsoluteReference with CurrentSection {
      override def encodableForPosition(position: Int): Resource with Encodable = encodableFactory(position)

      override def target: Label = targetLabel
      override def label: Label = thisLabel

      override def minimumSize: Int = initialMinimumSize
      override def maximumSize: Int = initialMaximumSize
    }

  def apply(targetSection: String, targetLabel: Label, initialMinimumSize: Int, initialMaximumSize: Int, thisLabel: Label,
    encodableFactory: (Int)=>Resource with Encodable) =
    new AbsoluteReference with OtherSection {
      override val sectionName: String = targetSection

      override def encodableForPosition(position: Int): Resource with Encodable = encodableFactory(position)

      override def target: Label = targetLabel
      override def label: Label = thisLabel

      override def minimumSize: Int = initialMinimumSize
      override def maximumSize: Int = initialMaximumSize
    }
}
