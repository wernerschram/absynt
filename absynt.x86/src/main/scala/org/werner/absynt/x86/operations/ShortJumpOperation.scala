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

package org.werner.absynt.x86.operations

import org.werner.absynt._
import org.werner.absynt.resource.{RelativeReference, Resource, UnlabeledEncodable}

abstract class ShortJumpOperation
  (val shortOpcode: Seq[Byte], mnemonic: String, targetLabel: Label)
    extends RelativeReference() {

  override def target: Label = targetLabel

  val shortJumpSize: Int = shortOpcode.length + 1

  def encodableForShortPointer(Offset: Byte): Resource with UnlabeledEncodable

  override def toString = s"$mnemonic $target"

  override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Resource with UnlabeledEncodable =
  {
    val offset = offsetDirection match {
      case OffsetDirection.Self => -shortJumpSize
      case OffsetDirection.Forward => distance
      case OffsetDirection.Backward => -distance - shortJumpSize
    }
    val offsetShort: Byte = offset.toByte
    assume(offsetShort == offset)
    encodableForShortPointer(offsetShort)
  }

  override def sizeForDependencySize(distance: Int, offsetDirection: OffsetDirection): Int =
    encodableForDependencySize(distance, offsetDirection).size

  override def possibleSizes: Set[Int] = Set(shortJumpSize)
}
