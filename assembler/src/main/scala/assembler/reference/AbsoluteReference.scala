package assembler.reference

import assembler.{Application, Encodable, Label, Resource}

sealed trait AbsoluteReference
    extends Resource {
  def target: Label

  def encodableForPosition(position: Int): Resource with Encodable

  def toInSectionState(application: Application): Resource = {
    val newMinimum = application.getAbsoluteMinimumAddress(target)
    val newMaximum = application.getAbsoluteMaximumAddress(target)
    if (newMinimum == newMaximum)
      encodableForPosition(newMinimum.toInt)
    else
      new AbsoluteReference {
        override def encodableForPosition(position: Int): Resource with Encodable =
          AbsoluteReference.this.encodableForPosition(position)

        override def target: Label = AbsoluteReference.this.target

        override def label: Label = AbsoluteReference.this.label

        override def minimumSize: Int = encodableForPosition(newMinimum.toInt).size

        override def maximumSize: Int = encodableForPosition(newMaximum.toInt).size
      }
  }
}

object AbsoluteReference {
  def apply(targetLabel: Label, initialMinimumSize: Int, initialMaximumSize: Int, thisLabel: Label,
    encodableFactory: (Int)=>Resource with Encodable) =
    new AbsoluteReference {
      override def encodableForPosition(position: Int): Resource with Encodable = encodableFactory(position)

      override def target: Label = targetLabel
      override def label: Label = thisLabel

      override def minimumSize: Int = initialMinimumSize
      override def maximumSize: Int = initialMaximumSize
    }
}
