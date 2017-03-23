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
}