package org.werner.absynt

import org.werner.absynt.sections.Section
import org.scalatest.{Matchers, WordSpec}

class EncodedBytesSuite extends WordSpec with Matchers {

  "an Encoded Byte List" should {
    "correctly encode 0x00, 0x01, 0x02" in {
      val value = EncodedBytes(0x00.toByte :: 0x01.toByte :: 0x02.toByte :: Nil)
      value.encodeByte should be(0x00.toByte :: 0x01.toByte :: 0x02.toByte :: Nil)
      value.size should be (3)
    }
  }
}