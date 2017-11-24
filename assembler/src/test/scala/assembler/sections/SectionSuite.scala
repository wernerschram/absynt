package assembler.sections

import assembler._
import assembler.output.raw.Raw
import assembler.reference.SinglePassRelativeReference
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}

class SectionSuite extends WordSpec with Matchers with MockFactory {

  abstract class TestOffset extends Offset {
    def offset: Long
  }

  case class TestRelativeOffset(override val offset: Long) extends TestOffset with RelativeOffset {
    def direction: OffsetDirectionOld =
      if (offset == 0)
        OffsetDirectionOld.None
      else if (offset < 0)
        OffsetDirectionOld.Backward
      else
        OffsetDirectionOld.Forward
  }

  class TestAddress(val offset: TestOffset) extends Address[TestOffset] {
    override def toLong: Long = offset.offset
  }

  val zeroAddress: TestAddress = new TestAddress(new TestRelativeOffset(0))

  // FIXME: mock[Application[TestOffset]] doesn't work

  implicit val offsetFactory: OffsetFactory[TestOffset] = new OffsetFactory[TestOffset] {
    override def offset(offsetValue: Long): TestOffset with RelativeOffset = new TestRelativeOffset(offsetValue)
    override def add(thisOffset: TestOffset, that: TestOffset with RelativeOffset): TestOffset with RelativeOffset = offset(thisOffset.offset + that.offset)
    override def add(thisOffset: TestOffset, that: Long): TestOffset with RelativeOffset = offset(thisOffset.offset + that)

    override def positionalOffset(offsetValue: Long)(offsetDirection: OffsetDirectionOld)(instructionSize: Int): TestOffset with TestRelativeOffset = ???
  }

  implicit val addressFactory: AddressFactory[TestOffset, TestAddress] = new AddressFactory[TestOffset, TestAddress] {
    override def zero = new TestAddress(new TestRelativeOffset(0))

    override def add(address: TestAddress, offset: TestOffset with RelativeOffset) =
      new TestAddress(offsetFactory.add(address.offset, offset))
  }

  "a Section" when {
    "queried for immediate instructions" should {


      "provide the intermediate instructions between a relative instruction and a label" in {
        val label = Label.unique
        val myRelativeReference: SinglePassRelativeReference[TestRelativeOffset] = mock[SinglePassRelativeReference[TestRelativeOffset]]
        (myRelativeReference.target _).expects().returning(label).anyNumberOfTimes()
        (myRelativeReference.label _).expects().returning(NoLabel()).anyNumberOfTimes()
        val reference = myRelativeReference
        val intermediate = EncodedByteList(List.fill(5)(0))
        val target = EncodedByteList(0.toByte :: Nil)(label)

        val section = Section[TestOffset](SectionType.Text, ".test", List[Resource](
          reference,
          intermediate,
          target))

        section.intermediateEncodables(reference) should be(intermediate :: Nil)
      }

      "provide the intermediate instructions between a label and a relative instruction" in {
        val label = Label.unique
        val reference: SinglePassRelativeReference[TestRelativeOffset] = mock[SinglePassRelativeReference[TestRelativeOffset]]
        (reference.target _).expects().returning(label).anyNumberOfTimes()
        (reference.label _).expects().returning(NoLabel()).anyNumberOfTimes()
        val intermediate = EncodedByteList(List.fill(5)(0))
        val target = EncodedByteList(0.toByte :: Nil)(label)

        val section = Section[TestOffset](SectionType.Text, ".test", List[Resource](
          target,
          intermediate,
          reference))

        section.intermediateEncodables(reference) should be(target :: intermediate :: Nil)
      }

      "return an empty list for an instruction that references itself" in {
        val targetLabel = Label.unique
        val reference: SinglePassRelativeReference[TestRelativeOffset] = mock[SinglePassRelativeReference[TestRelativeOffset]]
        (reference.target _).expects().returning(targetLabel).anyNumberOfTimes()
        (reference.label _).expects().returning(targetLabel).anyNumberOfTimes()
        val prefix = EncodedByteList(List.fill(2)(0))
        val postfix = EncodedByteList(List.fill(3)(0))

        val section = Section[TestOffset](SectionType.Text, ".test", List[Resource](
          prefix,
          reference,
          postfix))

        section.intermediateEncodables(reference) should be(Nil)
      }
    }

    "queried for a references direction" should {

      "know when a indirect reference is a forward reference" in {
        val label = Label.unique
        val reference: SinglePassRelativeReference[TestOffset] = mock[SinglePassRelativeReference[TestOffset]]
        (reference.target _).expects().returning(label).anyNumberOfTimes()
        (reference.label _).expects().returning(NoLabel()).anyNumberOfTimes()
        val target = EncodedByteList(0.toByte :: Nil)(label)

        val section = Section[TestOffset](SectionType.Text, ".test", List[Resource](
          reference,
          target))

        section.offsetDirectionOld(reference) shouldBe OffsetDirectionOld.Forward
      }

      "know when a indirect reference is a backward reference" in {
        val label = Label.unique
        val reference: SinglePassRelativeReference[TestOffset] = mock[SinglePassRelativeReference[TestOffset]]
        (reference.target _).expects().returning(label).anyNumberOfTimes()
        (reference.label _).expects().returning(NoLabel()).anyNumberOfTimes()
        val target = EncodedByteList(0.toByte :: Nil)(label)

        val section = Section[TestOffset](SectionType.Text, ".test", List[Resource](
          target,
          reference))

        section.offsetDirectionOld(reference) shouldBe OffsetDirectionOld.Backward
      }

      "know when a indirect reference is a reference to self" in {
        val label = Label.unique
        val reference: SinglePassRelativeReference[TestOffset] = mock[SinglePassRelativeReference[TestOffset]]
        (reference.target _).expects().returning(label).anyNumberOfTimes()
        (reference.label _).expects().returning(label).anyNumberOfTimes()

        val section = Section[TestOffset](SectionType.Text, ".test", List[Resource](
          reference))

        section.offsetDirectionOld(reference) shouldBe OffsetDirectionOld.None
      }

    }

    "asked to encode itself" should {

      "be able to encode itself" in {
        val section = Section[TestOffset](SectionType.Text, ".test", List[Resource](
          EncodedByteList(0x00.toByte :: 0x01.toByte :: Nil),
          EncodedByteList(0xEF.toByte :: 0xFF.toByte :: Nil)))

        val application: Application[TestOffset, TestAddress] = Raw[TestOffset, TestAddress](section, zeroAddress)
        application.encodableSections.head.encodeByte should be(0x00.toByte :: 0x01.toByte :: 0xEF.toByte :: 0xFF.toByte :: Nil)
      }
    }

    "queried for its size" should {

      "correctly calculate its size" in {
        val oneSize = 4
        val twoSize = 6
        val one = EncodedByteList(List.fill(oneSize)(1))
        val two = EncodedByteList(List.fill(twoSize)(2))

        val section = Section[TestOffset](SectionType.Text, ".test", List[Resource](
          one,
          two))

        val application: Application[TestOffset, TestAddress] = Raw[TestOffset, TestAddress](section, zeroAddress)
        application.encodableSections.head.size should be(oneSize + twoSize)
      }
    }

    "queried for the relative address of a label" should {

      "correctly provide the section relative address of a label " in {
        val label = Label.unique
        val intermediate = EncodedByteList(List.fill(5)(0))
        val target = EncodedByteList(0.toByte :: Nil)(label)

        val section = Section[TestOffset](SectionType.Text, ".test", List[Resource](
          intermediate,
          target))

        val application: Application[TestOffset, TestAddress] = Raw[TestOffset, TestAddress](section, zeroAddress)
        application.encodableSections.head.offset[TestRelativeOffset](target) should be(TestRelativeOffset(5))
      }
    }
  }
}