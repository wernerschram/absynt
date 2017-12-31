package assembler.output.raw

import assembler._
import assembler.resource._
import assembler.sections.{LastIteration, Section}

class Raw(section: Section, override val startOffset: Int)
  extends Application {

  override val sections: List[Section] = section :: Nil

  override val alignmentFillers: Map[Section, AlignmentFiller] = Map(section -> AlignmentFiller(section))

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
