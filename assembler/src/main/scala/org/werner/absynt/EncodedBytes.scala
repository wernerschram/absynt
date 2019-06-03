package org.werner.absynt

import org.werner.absynt.ListExtensions._
import org.werner.absynt.resource.UnlabeledEncodable

case class EncodedBytes(bytes: Seq[Byte]) extends UnlabeledEncodable {

  def encodeByte: Seq[Byte] = bytes

  def size: Int = bytes.length

  override def toString: String = s"""SETB "${bytes.bigEndianHexString}""""
}

object EncodedBytes {
  def apply(byte: Byte): EncodedBytes = new EncodedBytes(Seq(byte))
}
