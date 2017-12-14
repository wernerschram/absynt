package assembler.output.raw

import assembler._
import assembler.resource._
import assembler.sections.{LastIteration, Section}

class Raw(section: Section, override val startOffset: Int)
  extends Application {

  override val sections: List[Section] = section :: Nil

  override val alignmentFillers: Map[Section, AlignmentFiller] = Map(section -> new AlignmentFiller(Label.noLabel) {

    override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int = startOffset

    override def possibleSizes: Set[Int] = Set(startOffset)

    override def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable =
      EncodedByteList(Seq.fill(startOffset)(0.toByte))(label)

    override def toString: String = s"filler for ${section.name}"

    override def section: Section = Raw.this.section
  })

  override def sectionOffset(section: Section with LastIteration): Long = startOffset

  override def encodeByte: List[Byte] = encodableSections.head.encodeByte

  def initialResources: List[Resource] = Nil
}

object Raw {
  def apply(section: Section, startOffset: Int) = new Raw(section, startOffset)
}
