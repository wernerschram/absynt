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

package org.werner.absynt.output.raw

import org.werner.absynt._
import org.werner.absynt.resource.EncodableConversion._
import org.werner.absynt.resource._
import org.werner.absynt.sections.Section

class Raw(section: Section, override val startOffset: Int)
  extends Application {

  override val sections: List[Section] = section :: Nil

  override val alignmentFillers: Map[Section, AlignmentFiller] = Map(section -> AlignmentFiller(section))

  def encodeByte: Seq[Byte] = {
    val map = encodablesForDependencies(section.content.dependentResources)
    val finalContent = section.content.encodables(map)
    finalContent.encodeByte
  }
}

object Raw {
  def apply(section: Section, startOffset: Int) = new Raw(section, startOffset)
}
