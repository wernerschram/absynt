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

import org.werner.absynt.ListExtensions._
import org.werner.absynt.resource.UnlabeledEncodable

class EncodedBytes(encodeBytes: => Seq[Byte], val size: Int) extends UnlabeledEncodable {
  override def toString: String = s"""SETB "${encodeBytes.bigEndianHexString}""""

  override def encodeByte: Seq[Byte] = encodeBytes
}

object EncodedBytes {
  def apply(bytes: => Seq[Byte]) = new EncodedBytes(bytes, bytes.length)

  def fill(size: Int, value: Byte) = new EncodedBytes(List.fill(size)(value), size)

  def apply(byte: Byte): EncodedBytes = new EncodedBytes(Seq(byte), 1)
}
