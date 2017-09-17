package assembler

import org.scalatest.Matchers
import org.scalatest.WordSpec
import assembler.sections.Section

class EncodedStringSuite extends WordSpec with Matchers {

  "an Encoded String" should {
    "correctly encode \"Test\"" in {
      val value = EncodedString("Test")
      value.encodeByte should be("Test".getBytes.toList)
      value.size should be (4)
    }
  }
}