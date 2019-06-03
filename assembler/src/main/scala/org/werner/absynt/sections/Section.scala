package org.werner.absynt.sections

import org.werner.absynt._
import org.werner.absynt.resource.{Labeled, RelativeReference, Resource}

import scala.language.implicitConversions

sealed abstract class Section protected(val content: Seq[Resource], val name: String, val alignment: Int) {

  def precedingResources(target: Label): Seq[Resource] = content.takeWhile(!matchLabel(_, target))

  private def matchResourceOrLabel(resource: Resource, target: Resource, label: Label): Boolean =
    (resource == target) || (resource match {
      case l: Labeled => l.label.matches(label) || l.resource == target
      case _ => false
    })

  private def matchResourceAndLabel(resource: Resource, target: Resource, label: Label): Boolean =
    resource match {
      case l: Labeled => l.label.matches(label) && l.resource == target
      case _ => false
    }

  private def matchLabel(resource: Resource, label: Label): Boolean =
    resource match {
      case l: Labeled => l.label.matches(label)
      case _ => false
    }

  private def matchResource(resource: Resource, target: Resource): Boolean =
    (resource == target) || (resource match {
      case l: Labeled => l.resource == target
      case _ => false
    })

  /** returns all resources between a relative reference and it's target. If it is a back reference, it will include the target
    *
    * @param from the source relative reference
    * @return the intermediate resources
    */
  def intermediateResources(from: RelativeReference): Seq[Resource] = {

    val trimLeft = content
      .dropWhile(x => !matchResourceOrLabel(x, from, from.target))

    if (matchResourceAndLabel(trimLeft.head, from, from.target))  // reference to self
      return Nil

    val trimRight = trimLeft.tail
      .takeWhile(x => !matchResourceOrLabel(x, from, from.target))

    if (matchResource(trimLeft.head, from))
      trimRight
    else
      trimLeft.head +: trimRight
  }

  def offsetDirection(from: RelativeReference): OffsetDirection = {
    val firstInstruction = content.find(x => matchResourceOrLabel(x, from, from.target)).get
    if (matchLabel(firstInstruction, from.target))
      if (matchResource(firstInstruction, from))
        OffsetDirection.Self
      else
        OffsetDirection.Backward
    else
      OffsetDirection.Forward
  }
}

final case class TextSection private[sections](override val content: Seq[Resource], override val name: String, override val alignment: Int) extends Section(content, name, alignment)
final case class DataSection private[sections](override val content: Seq[Resource], override val name: String, override val alignment: Int) extends Section(content, name, alignment)

object Section {
  def text(resources: Seq[Resource], sectionName: String = ".text", alignment: Int = 16): Section =
    TextSection(resources, sectionName, alignment)

  def data(resources: Seq[Resource], sectionName: String = ".data", alignment: Int = 16): Section =
    DataSection(resources, sectionName, alignment)
}
