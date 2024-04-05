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

package org.werner.absynt.arm.operations

import org.werner.absynt.arm.operands.{Condition, RightRotateImmediate}
import org.werner.absynt.arm.operands.registers._

import scala.language.implicitConversions

class MoveFromStatusRegister(override val opcode: String, source: StatusRegister, destination: GeneralRegister,
                             override val condition: Condition)
  extends Conditional {
  override def encodeWord: Int =
    super.encodeWord | 0x010f0000 | (source.registerCode << 22) | (destination.registerCode << 12)

  override def toString: String = s"$mnemonicString ${destination.toString}, ${source.toString}"
}

object Fields extends Enumeration {
  type Fields = Value

  val control: Value = Value(16, "c")
  val extension: Value = Value(17, "x")
  val status: Value = Value(18, "s")
  val flags: Value = Value(19, "f")

  def fieldsToString(set: ValueSet): String =
    set.foldRight(new StringBuilder)((value, builder) => builder.addAll(value.toString)).result()
}

class MoveToStatusRegister private(override val opcode: String, destination: StatusRegister, fields: Fields.ValueSet,
                                   override val condition: Condition, val sourceString: String, val sourceValue: Int)
  extends Conditional {

  def this(opcode: String, source: GeneralRegister, destination: StatusRegister, fields: Fields.ValueSet,
           condition: Condition) =
    this(opcode, destination, fields, condition, source.toString, source.registerCode)

  def this(opcode: String, source: RightRotateImmediate, destination: StatusRegister, fields: Fields.ValueSet,
           condition: Condition) =
    this(opcode, destination, fields, condition, source.toString, source.encode)

  override def encodeWord: Int =
    super.encodeWord | 0x0120f000 | (destination.registerCode << 22 | fields.toBitMask(0).toInt | sourceValue)

  override def toString: String = s"$mnemonicString ${destination.toString}_${Fields.fieldsToString(fields)}, $sourceString"
}
