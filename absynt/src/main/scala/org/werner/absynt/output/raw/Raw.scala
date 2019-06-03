package org.werner.absynt.output.raw

import org.werner.absynt._
import org.werner.absynt.resource.EncodableConversion._
import org.werner.absynt.resource._
import org.werner.absynt.sections.Section

class Raw(section: Section, override val startOffset: Int)
  extends Application {

  override val sections: List[Section] = section :: Nil

  override val alignmentFillers: Map[Section, AlignmentFiller] = Map(section -> AlignmentFiller(section))

  override def encodeByte: Seq[Byte] = {
    val map = encodablesForDependencies(section.content.dependentResources)
    val finalContent = section.content.encodables(map)
    finalContent.encodeByte
  }
}

object Raw {
  def apply(section: Section, startOffset: Int) = new Raw(section, startOffset)
}
