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

