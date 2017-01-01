package assembler

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec
import assembler.memory.MemoryPage

class EncodedStringSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[Encodable])

  "an Encoded String" should {
    "correctly encode \"Test\"" in {
      val value = EncodedString("Test")
      value.encodeByte should be("Test".getBytes.toList)
      value.size should be (4)
    }
  }
  
  "a Labeled Encoded String" should {
    "correctly encode \"Labeled Test\"" in {
      val value = EncodedString("Labeled Test").withLabel("Test")
      value.encodeByte should be("Labeled Test".getBytes.toList)
      value.size should be (12)
    }
  }
}