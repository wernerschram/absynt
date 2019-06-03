package org.werner.absynt.resource

import org.werner.absynt.{Application, Label, OffsetDirection}
import org.werner.absynt.sections.Section
import EncodableConversion._

abstract class AbsoluteReference(val target: Label) extends UnlabeledDependentResource {

  def encodableForDistance(distance: Int): UnlabeledEncodable

  final override def unlabeledForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): UnlabeledEncodable = {
    assume(offsetDirection == OffsetDirection.Absolute)
    encodableForDistance(dependencySize)
  }

  def sizeForDistance(distance: Int): Int

  final override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int = {
    assume(offsetDirection == OffsetDirection.Absolute)
    sizeForDistance(dependencySize)
  }

  override def dependencies(context: Application): (Seq[Resource], OffsetDirection) = {
    val containingSection: Section = context.sections.filter(s => s.content.containsLabel(target)).head
    (
      context.startFiller +: (context.alignedSectionDependencies(containingSection) ++
      containingSection.precedingResources(target))
       ,OffsetDirection.Absolute
    )
  }
}

