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

package org.werner.absynt.x86.operations.branch

import org.werner.absynt.{Label, OffsetDirection, RelativeOffsetDirection}
import org.werner.absynt.resource.{RelativeReference, Resource, UnlabeledEncodable}

abstract class JumpOption(val encodedLength: Int, val minValue: Int, val maxValue: Int) {
  def encodableForPointer(offset: Int): Resource with UnlabeledEncodable
}

case class LabelJumpOperation(ascendingSizeOptions: Seq[JumpOption], mnemonic: String, target: Label)
    extends RelativeReference() {

  override def possibleSizes: Set[Int] = ascendingSizeOptions.map(_.encodedLength).toSet

  override def toString = s"$mnemonic $target"

  def offset(jumpSize: Int, distance: Int, offsetDirection: RelativeOffsetDirection): Int = offsetDirection match {
    case OffsetDirection.Self => -jumpSize
    case OffsetDirection.Forward => distance
    case OffsetDirection.Backward => -distance - jumpSize
  }

  override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Resource with UnlabeledEncodable = {
    ascendingSizeOptions.map{option =>
      val thisOffset = offset(option.encodedLength, distance, offsetDirection)
      if (thisOffset >= option.minValue && thisOffset <= option.maxValue) {
        Some(option.encodableForPointer(thisOffset))
      } else None
    }.collectFirst{case Some(v) => v}.getOrElse(throw new AssertionError())
  }

  override def sizeForDependencySize(distance: Int, offsetDirection: OffsetDirection): Int =
    encodableForDependencySize(distance, offsetDirection).size
}
