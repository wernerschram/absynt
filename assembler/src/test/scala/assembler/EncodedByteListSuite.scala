package assembler

import assembler.sections.Section
import org.scalatest.{Matchers, WordSpec}

class EncodedByteListSuite extends WordSpec with Matchers {

  implicit val page: Section = Section(List.empty[Encodable])

  "an Encoded Byte List" should {
    "correctly encode 0x00, 0x01, 0x02" in {
      val value = EncodedByteList(0x00.toByte :: 0x01.toByte :: 0x02.toByte :: Nil)
      value.encodeByte should be(0x00.toByte :: 0x01.toByte :: 0x02.toByte :: Nil)
      value.size should be (3)
    }
  }
  
  "a Labeled Encoded Byte List" should {
    "correctly encode Test: 0xFF, 0xFE" in {
      val value = EncodedByteList(0xFF.toByte :: 0xFE.toByte :: Nil).withLabel("Test")
      value.encodeByte should be(0xFF.toByte :: 0xFE.toByte :: Nil)
      value.size should be (2)
    }
  }
}