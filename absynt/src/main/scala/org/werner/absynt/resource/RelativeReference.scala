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

import org.werner.absynt.{Application, Label, OffsetDirection, RelativeOffsetDirection}

abstract class RelativeReference() extends UnlabeledDependentResource {

  def target: Label

  final def unlabeledForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): UnlabeledEncodable = {
    assume(offsetDirection.isInstanceOf[RelativeOffsetDirection])
    encodableForDistance(dependencySize, offsetDirection.asInstanceOf[RelativeOffsetDirection])
  }

  def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): UnlabeledEncodable

  def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int

  override def dependencies(context: Application): (Seq[Resource], OffsetDirection) = {
    val section = context.sections.filter(s => s.content.exists(r =>
      (r == this) || (r.isInstanceOf[Labeled] && r.asInstanceOf[Labeled].resource == this)
    )).head

    (section.intermediateResources(this), section.offsetDirection(this))
  }
}

