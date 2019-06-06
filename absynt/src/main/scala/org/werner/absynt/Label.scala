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

package org.werner.absynt

import scala.language.implicitConversions

abstract class Label {
  def matches(label: Label): Boolean
}

object Label {
  implicit def apply(value: String): Label = StringLabel(value)

  def unique: UniqueLabel = synchronized {
    lastId += 1
    UniqueLabel(lastId)
  }

  private var lastId = 0
}

case class StringLabel private (value: String) extends Label {
  def matches(label: Label): Boolean = this.equals(label)

  override def toString: String = value
}

case class UniqueLabel private (id: Int) extends Label {
  def matches(label: Label): Boolean = this.equals(label)

  override def toString: String = s"__$id"
}
