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

trait Repeated {
  self: X86Operation & ModRMBytes & DisplacementBytes & ImmediateBytes =>
  override val optionalRepeatPrefix: List[Byte] = 0xF3.toByte :: Nil
  override val optionalRepeatPrefixString = "rep "
}

trait RepeatEqual {
  self: X86Operation & ModRMBytes & DisplacementBytes & ImmediateBytes =>
  override val optionalRepeatPrefix: List[Byte] = 0xF3.toByte :: Nil
  override val optionalRepeatPrefixString = "repe "
}

trait RepeatNotEqual {
  self: X86Operation & ModRMBytes & DisplacementBytes & ImmediateBytes =>
  override val optionalRepeatPrefix: List[Byte] = 0xF2.toByte :: Nil
  override val optionalRepeatPrefixString = "repne "
}
