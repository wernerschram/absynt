/*
 * Copyright 2019 Werner Schram
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

package org.werner.absynt.sections

import org.werner.absynt._
import org.werner.absynt.resource.{Labeled, RelativeReference, Resource}


abstract class Section protected(val name: String, val alignment: Int) {

  val content: Seq[Resource]

  def precedingResources(target: Label): Seq[Resource] = content.takeWhile(!matchLabel(_, target))

  private def matchResourceOrLabel(resource: Resource, target: Resource, label: Label): Boolean =
    resource == target || resource.isInstanceOf[Labeled] && {
      val l = resource.asInstanceOf[Labeled]
      l.label.matches(label) || l.resource == target
    }

  private def matchResourceAndLabel(resource: Resource, target: Resource, label: Label): Boolean =
    resource.isInstanceOf[Labeled] && {
      val l = resource.asInstanceOf[Labeled]
      l.label.matches(label) && l.resource == target
    }

  private def matchLabel(resource: Resource, label: Label): Boolean =
    resource.isInstanceOf[Labeled] && resource.asInstanceOf[Labeled].label.matches(label)

  private def matchResource(resource: Resource, target: Resource): Boolean =
    (resource == target) || resource.isInstanceOf[Labeled] && resource.asInstanceOf[Labeled].resource == target

  /** returns all resources between a relative reference and it's target. If it is a back reference, it will include the target
    *
    * @param from the source relative reference
    * @return the intermediate resources
    */
  def intermediateResources(from: RelativeReference): Seq[Resource] = {

    val trimLeft = content
      .dropWhile(x => !matchResourceOrLabel(x, from, from.target))

    if matchResourceAndLabel(trimLeft.head, from, from.target) then  // reference to self
      return Nil

    val trimRight = trimLeft.tail
      .takeWhile(x => !matchResourceOrLabel(x, from, from.target))

    if matchResource(trimLeft.head, from) then
      trimRight
    else
      trimLeft.head +: trimRight
  }

  def offsetDirection(from: RelativeReference): OffsetDirection = {
    val firstInstruction = content.find(x => matchResourceOrLabel(x, from, from.target)).get
    if matchLabel(firstInstruction, from.target) then
      if matchResource(firstInstruction, from) then
        OffsetDirection.Self
      else
        OffsetDirection.Backward
    else
      OffsetDirection.Forward
  }
}

final case class TextSection private[sections](override val content: Seq[Resource], override val name: String, override val alignment: Int) extends Section(name, alignment)
final case class DataSection private[sections](override val content: Seq[Resource], override val name: String, override val alignment: Int) extends Section(name, alignment)

object Section {
  def text(resources: Seq[Resource], sectionName: String = ".text", alignment: Int = 16): Section =
    TextSection(resources, sectionName, alignment)

  def data(resources: Seq[Resource], sectionName: String = ".data", alignment: Int = 16): Section =
    DataSection(resources, sectionName, alignment)
}
