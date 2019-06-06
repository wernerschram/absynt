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

import org.werner.absynt.sections.Section
import org.werner.absynt.{Application, EncodedBytes, OffsetDirection}

final case class AlignmentFiller(section: Section) extends UnlabeledDependentResource {

  def dependencies(context: Application): (Seq[Resource], OffsetDirection) =
    (context.startFiller +: context.sectionDependencies(section), OffsetDirection.Absolute)

  override def unlabeledForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): UnlabeledEncodable =
    EncodedBytes(Seq.fill(sizeForDependencySize(dependencySize, offsetDirection))(0.toByte))

  override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int = {
    val alignment = dependencySize % section.alignment
    if (alignment != 0)
      section.alignment - alignment
    else 0
  }

  override def possibleSizes: Set[Int] = (0 to section.alignment by 1).toSet

  override def toString: String = s"filler for ${section.name}"
}

