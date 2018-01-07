package assembler.output.raw

import assembler._
import assembler.resource.EncodableConversion._
import assembler.resource._
import assembler.sections.Section

class Raw(section: Section, override val startOffset: Int)
  extends Application {

  override val sections: List[Section] = section :: Nil

  override val alignmentFillers: Map[Section, AlignmentFiller] = Map(section -> AlignmentFiller(section))

  override def encodeByte: Seq[Byte] = {
    val map = encodablesForReferences(section.content.dependentResources)
    val finalContent = section.content.encodables(map)
    finalContent.encodeByte.toList
  }

  def initialResources: List[Resource] = Nil
}

object Raw {
  def apply(section: Section, startOffset: Int) = new Raw(section, startOffset)
}
