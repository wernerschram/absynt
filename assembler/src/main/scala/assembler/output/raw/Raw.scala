package assembler.output.raw

import assembler._
import assembler.resource._
import assembler.sections.{LastIteration, Section}

class Raw(section: Section, override val startOffset: Int)
  extends Application {

  override val sections: List[Section] = section :: Nil

  override val alignmentFillers: Map[Section, AlignmentFiller] = Map(section -> new AlignmentFiller(Label.noLabel) {

    override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int = {
      val alignment = dependencySize % section.alignment
      if (alignment != 0)
        section.alignment - alignment
      else 0
    }

    override def possibleSizes: Set[Int] = (0 to section.alignment by 1).toSet

    override def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable =
      EncodedByteList(Seq.fill(sizeForDependencySize(dependencySize, offsetDirection))(0.toByte))(label)

    override def toString: String = s"filler for ${section.name}"

    override def section: Section = Raw.this.section
  })

  override def encodeByte: List[Byte] = {
    val map = encodablesForReferences(section.content.collect{case r: DependentResource => r} ::: alignmentFillers.values.toList)
    val finalSection = encodableSection(section, map)
    finalSection.encodeByte
  }

  def initialResources: List[Resource] = Nil
}

object Raw {
  def apply(section: Section, startOffset: Int) = new Raw(section, startOffset)
}
