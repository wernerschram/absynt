package org.werner.absynt.resource

import org.werner.absynt.{Application, Label, OffsetDirection, RelativeOffsetDirection}

abstract class RelativeReference(val target: Label) extends UnlabeledDependentResource {

  final def unlabeledForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): UnlabeledEncodable = {
    assume(offsetDirection.isInstanceOf[RelativeOffsetDirection])
    encodableForDistance(dependencySize, offsetDirection.asInstanceOf[RelativeOffsetDirection])
  }

  def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): UnlabeledEncodable

  def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int

  override def dependencies(context: Application): (Seq[Resource], OffsetDirection) = {
    val section = context.sections.filter(s => s.content.exists(r =>
        (r == this) || (r match {
          case l: Labeled => l.resource == this
          case _ => false
        }))).head

    (section.intermediateResources(this), section.offsetDirection(this))
  }
}

