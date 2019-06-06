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

import org.werner.absynt.Label

object EncodableConversion {
  implicit class Resources(resources: Seq[Resource]) {
    def encodables(dependentMap: Map[DependentResource, Encodable]): Seq[Encodable] = resources.map {
      case reference: DependentResource => dependentMap(reference)
      case encodable: Encodable => encodable
    }

    def dependentResources: Seq[DependentResource] = resources.collect{case r: DependentResource => r}

    def containsLabel(label: Label): Boolean =
      resources.collect{ case r: Labeled => r}.exists(_.label.matches(label))
  }

  implicit class Encodables(encodables: Seq[Encodable]) {
    def encodeByte: Seq[Byte] = encodables.flatMap { x => x.encodeByte }
  }
}
