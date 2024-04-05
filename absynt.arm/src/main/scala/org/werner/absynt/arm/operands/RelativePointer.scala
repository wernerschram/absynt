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

package org.werner.absynt.arm.operands

import org.werner.absynt.ListExtensions._
import org.werner.absynt.{OffsetDirection, RelativeOffsetDirection}

import scala.language.implicitConversions

abstract class ArmOffset protected(val offset: Int) {
  override def toString: String = s"0x${(offset + 8).encodeBigEndian.hexString}"
}

trait RelativeOffset {
  self: ArmOffset =>
}

sealed case class ArmRelativeOffset(override val offset: Int) extends ArmOffset(offset) with RelativeOffset

object ArmRelativeOffset {
  def positionalOffset(offsetValue: Long)(offsetDirection: RelativeOffsetDirection): ArmOffset & RelativeOffset =
    offsetDirection match {
      case OffsetDirection.Self => ArmRelativeOffset(-8)
      case OffsetDirection.Forward => ArmRelativeOffset((offsetValue - 4).toInt)
      case OffsetDirection.Backward => ArmRelativeOffset((-offsetValue - 8).toInt)
    }

}

sealed abstract class RelativePointer(val offset: ArmOffset & RelativeOffset) extends Operand {
  //offset should be between 3221225472 and -3221225473
  assume(((offset.offset & 0xC0000000) == 0) || ((offset.offset & 0xC0000000) == 0xC0000000))

  def encode: Int = (offset.offset >> 2) & 0xFFFFFF

  override def toString: String = offset.toString
}

case class RelativeA32Pointer(override val offset: ArmOffset & RelativeOffset) extends RelativePointer(offset) {
  //offset should be divisible by 4
  assume((offset.offset & 0x00000003) == 0)
}

case class RelativeThumbPointer(override val offset: ArmOffset & RelativeOffset) extends RelativePointer(offset) {
  //offset should be divisible by 2
  assume((offset.offset & 0x00000001) == 0)

  override def encode: Int = super.encode | ((offset.offset & 2) << 23)
}

