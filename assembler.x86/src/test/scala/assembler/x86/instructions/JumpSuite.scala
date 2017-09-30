package assembler.x86.instructions

import assembler._
import assembler.sections.{Section, SectionType}
import assembler.x86.ProcessorMode
import assembler.x86.operands.Register._
import assembler.x86.operands.memoryaccess._
import org.scalatest.{Matchers, WordSpec}

class JumpSuite extends WordSpec with Matchers {

  "an Jump instruction" when {
    "in real mode" should {

      implicit val processorMode: ProcessorMode = ProcessorMode.Real

      "correctly encode jmp 0x10" in { Jump(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("EB 10")) }
      "correctly encode ja 0x10" in { JumpIfAbove(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("77 10")) }
      "correctly encode jae 0x10" in { JumpIfAboveOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jb 0x10" in { JumpIfBelow(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode jbe 0x10" in { JumpIfBelowOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("76 10")) }
      "correctly encode jc 0x10" in { JumpIfCarry(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode je 0x10" in { JumpIfEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("74 10")) }
      "correctly encode jg 0x10" in { JumpIfGreater(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7F 10")) }
      "correctly encode jge 0x10" in { JumpIfGreaterOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7D 10")) }
      "correctly encode jl 0x10" in { JumpIfLess(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7C 10")) }
      "correctly encode jle 0x10" in { JumpIfLessOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7E 10")) }
      "correctly encode jna 0x10" in { JumpIfNotAbove(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("76 10")) }
      "correctly encode jnae 0x10" in { JumpIfNotAboveOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode jnb 0x10" in { JumpIfNotBelow(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jnbe 0x10" in { JumpIfNotBelowOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("77 10")) }
      "correctly encode jnc 0x10" in { JumpIfNoCarry(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jne 0x10" in { JumpIfNotEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("75 10")) }
      "correctly encode jng 0x10" in { JumpIfNotGreater(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7E 10")) }
      "correctly encode jnge 0x10" in { JumpIfNotGreaterOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7C 10")) }
      "correctly encode jnl 0x10" in { JumpIfNotLess(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7D 10")) }
      "correctly encode jnle 0x10" in { JumpIfNotLessOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7F 10")) }

      "correctly encode jmp 0x2030" in { Jump(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("E9 30 20")) }
      "correctly encode ja 0x2030" in { JumpIfAbove(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 87 30 20")) }
      "correctly encode jae 0x2030" in { JumpIfAboveOrEqual(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 83 30 20")) }
      "correctly encode jb 0x2030" in { JumpIfBelow(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 82 30 20")) }
      "correctly encode jbe 0x2030" in { JumpIfBelowOrEqual(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 86 30 20")) }
      "correctly encode jc 0x2030" in { JumpIfCarry(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 82 30 20")) }
      "correctly encode je 0x2030" in { JumpIfEqual(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 84 30 20")) }
      "correctly encode jg 0x2030" in { JumpIfGreater(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 8F 30 20")) }
      "correctly encode jge 0x2030" in { JumpIfGreaterOrEqual(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 8D 30 20")) }
      "correctly encode jl 0x2030" in { JumpIfLess(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 8C 30 20")) }
      "correctly encode jle 0x2030" in { JumpIfLessOrEqual(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 8E 30 20")) }
      "correctly encode jna 0x2030" in { JumpIfNotAbove(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 86 30 20")) }
      "correctly encode jnae 0x2030" in { JumpIfNotAboveOrEqual(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 82 30 20")) }
      "correctly encode jnb 0x2030" in { JumpIfNotBelow(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 83 30 20")) }
      "correctly encode jnbe 0x2030" in { JumpIfNotBelowOrEqual(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 87 30 20")) }
      "correctly encode jnc 0x2030" in { JumpIfNoCarry(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 83 30 20")) }
      "correctly encode jne 0x2030" in { JumpIfNotEqual(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 85 30 20")) }
      "correctly encode jng 0x2030" in { JumpIfNotGreater(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 8E 30 20")) }
      "correctly encode jnge 0x2030" in { JumpIfNotGreaterOrEqual(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 8C 30 20")) }
      "correctly encode jnl 0x2030" in { JumpIfNotLess(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 8D 30 20")) }
      "correctly encode jnle 0x2030" in { JumpIfNotLessOrEqual(LongPointer(0x2030.toShort)).encodeByte should be(Hex.lsb("0F 8F 30 20")) }

      "correctly represent jmp 0x10 as a string" in { Jump(ShortPointer(0x10.toByte)).toString should be("jmp 0x10") }
      "correctly represent ja 0x10 as a string" in { JumpIfAbove(ShortPointer(0x10.toByte)).toString should be("ja 0x10") }
      "correctly represent jae 0x10 as a string" in { JumpIfAboveOrEqual(ShortPointer(0x10.toByte)).toString should be("jae 0x10") }
      "correctly represent jb 0x10 as a string" in { JumpIfBelow(ShortPointer(0x10.toByte)).toString should be("jb 0x10") }
      "correctly represent jbe 0x10 as a string" in { JumpIfBelowOrEqual(ShortPointer(0x10.toByte)).toString should be("jbe 0x10") }
      "correctly represent jc 0x10 as a string" in { JumpIfCarry(ShortPointer(0x10.toByte)).toString should be("jc 0x10") }
      "correctly represent je 0x10 as a string" in { JumpIfEqual(ShortPointer(0x10.toByte)).toString should be("je 0x10") }
      "correctly represent jg 0x10 as a string" in { JumpIfGreater(ShortPointer(0x10.toByte)).toString should be("jg 0x10") }
      "correctly represent jge 0x10 as a string" in { JumpIfGreaterOrEqual(ShortPointer(0x10.toByte)).toString should be("jge 0x10") }
      "correctly represent jl 0x10 as a string" in { JumpIfLess(ShortPointer(0x10.toByte)).toString should be("jl 0x10") }
      "correctly represent jle 0x10 as a string" in { JumpIfLessOrEqual(ShortPointer(0x10.toByte)).toString should be("jle 0x10") }
      "correctly represent jna 0x10 as a string" in { JumpIfNotAbove(ShortPointer(0x10.toByte)).toString should be("jna 0x10") }
      "correctly represent jnae 0x10 as a string" in { JumpIfNotAboveOrEqual(ShortPointer(0x10.toByte)).toString should be("jnae 0x10") }
      "correctly represent jnb 0x10 as a string" in { JumpIfNotBelow(ShortPointer(0x10.toByte)).toString should be("jnb 0x10") }
      "correctly represent jnbe 0x10 as a string" in { JumpIfNotBelowOrEqual(ShortPointer(0x10.toByte)).toString should be("jnbe 0x10") }
      "correctly represent jnc 0x10 as a string" in { JumpIfNoCarry(ShortPointer(0x10.toByte)).toString should be("jnc 0x10") }
      "correctly represent jne 0x10 as a string" in { JumpIfNotEqual(ShortPointer(0x10.toByte)).toString should be("jne 0x10") }
      "correctly represent jng 0x10 as a string" in { JumpIfNotGreater(ShortPointer(0x10.toByte)).toString should be("jng 0x10") }
      "correctly represent jnge 0x10 as a string" in { JumpIfNotGreaterOrEqual(ShortPointer(0x10.toByte)).toString should be("jnge 0x10") }
      "correctly represent jnl 0x10 as a string" in { JumpIfNotLess(ShortPointer(0x10.toByte)).toString should be("jnl 0x10") }
      "correctly represent jnle 0x10 as a string" in { JumpIfNotLessOrEqual(ShortPointer(0x10.toByte)).toString should be("jnle 0x10") }

      "correctly represent jmp 0x2030 as a string" in { Jump(LongPointer(0x2030.toShort)).toString should be("jmp 0x2030") }
      "correctly represent ja 0x2030 as a string" in { JumpIfAbove(LongPointer(0x2030.toShort)).toString should be("ja 0x2030") }
      "correctly represent jae 0x2030 as a string" in { JumpIfAboveOrEqual(LongPointer(0x2030.toShort)).toString should be("jae 0x2030") }
      "correctly represent jb 0x2030 as a string" in { JumpIfBelow(LongPointer(0x2030.toShort)).toString should be("jb 0x2030") }
      "correctly represent jbe 0x2030 as a string" in { JumpIfBelowOrEqual(LongPointer(0x2030.toShort)).toString should be("jbe 0x2030") }
      "correctly represent jc 0x2030 as a string" in { JumpIfCarry(LongPointer(0x2030.toShort)).toString should be("jc 0x2030") }
      "correctly represent je 0x2030 as a string" in { JumpIfEqual(LongPointer(0x2030.toShort)).toString should be("je 0x2030") }
      "correctly represent jg 0x2030 as a string" in { JumpIfGreater(LongPointer(0x2030.toShort)).toString should be("jg 0x2030") }
      "correctly represent jge 0x2030 as a string" in { JumpIfGreaterOrEqual(LongPointer(0x2030.toShort)).toString should be("jge 0x2030") }
      "correctly represent jl 0x2030 as a string" in { JumpIfLess(LongPointer(0x2030.toShort)).toString should be("jl 0x2030") }
      "correctly represent jle 0x2030 as a string" in { JumpIfLessOrEqual(LongPointer(0x2030.toShort)).toString should be("jle 0x2030") }
      "correctly represent jna 0x2030 as a string" in { JumpIfNotAbove(LongPointer(0x2030.toShort)).toString should be("jna 0x2030") }
      "correctly represent jnae 0x2030 as a string" in { JumpIfNotAboveOrEqual(LongPointer(0x2030.toShort)).toString should be("jnae 0x2030") }
      "correctly represent jnb 0x2030 as a string" in { JumpIfNotBelow(LongPointer(0x2030.toShort)).toString should be("jnb 0x2030") }
      "correctly represent jnbe 0x2030 as a string" in { JumpIfNotBelowOrEqual(LongPointer(0x2030.toShort)).toString should be("jnbe 0x2030") }
      "correctly represent jnc 0x2030 as a string" in { JumpIfNoCarry(LongPointer(0x2030.toShort)).toString should be("jnc 0x2030") }
      "correctly represent jne 0x2030 as a string" in { JumpIfNotEqual(LongPointer(0x2030.toShort)).toString should be("jne 0x2030") }
      "correctly represent jng 0x2030 as a string" in { JumpIfNotGreater(LongPointer(0x2030.toShort)).toString should be("jng 0x2030") }
      "correctly represent jnge 0x2030 as a string" in { JumpIfNotGreaterOrEqual(LongPointer(0x2030.toShort)).toString should be("jnge 0x2030") }
      "correctly represent jnl 0x2030 as a string" in { JumpIfNotLess(LongPointer(0x2030.toShort)).toString should be("jnl 0x2030") }
      "correctly represent jnle 0x2030 as a string" in { JumpIfNotLessOrEqual(LongPointer(0x2030.toShort)).toString should be("jnle 0x2030") }

      "correctly encode jcx 0x10" in { JumpIfCountZero(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("E3 10")) }
      "throw an AssertionError for jcx 0x2030" in { an[AssertionError] should be thrownBy { JumpIfCountZero(LongPointer(0x2030)) } }
      "correctly represent jcx 0x10 as a string" in { JumpIfCountZero(ShortPointer(0x10.toByte)).toString should be("jcx 0x10") }

      "throw an AssertionError for jmp 0x10203040" in { an[AssertionError] should be thrownBy { Jump(LongPointer(0x10203040)).encodeByte } }

      "correctly encode jmp ax" in { Jump(AX).encodeByte should be(Hex.lsb("FF E0")) }
      "correctly represent jmp ax as a string" in { Jump(AX).toString should be("jmp ax") }

      "correctly encode jmp [bp+si]" in { Jump(RegisterMemoryLocation(BP.combinedIndex(SI))).encodeByte should be(Hex.lsb("FF 22")) }
      "correctly represent jmp [bp+si] as a string" in { Jump(RegisterMemoryLocation(BP.combinedIndex(SI))).toString should be("jmp [bp+si]") }

      "correctly encode jmp eax" in { Jump(EAX).encodeByte should be(Hex.lsb("66 FF E0")) }
      "correctly represent jmp eax as a string" in { Jump(EAX).toString should be("jmp eax") }

      "correctly encode jmp [eax]" in { Jump(RegisterMemoryLocation(EAX)).encodeByte should be(Hex.lsb("67 FF 20")) }
      "correctly represent jmp [eax] as a string" in { Jump(RegisterMemoryLocation(EAX)).toString should be("jmp [eax]") }

      "throw an AssertionError for jmp rax" in { an[AssertionError] should be thrownBy { Jump(RAX) } }

      "correctly encode jmp DWORD PTR fs:[bx+si]" in {
        Jump(RegisterMemoryLocation.withSegmentOverride.doubleWordSize(BX.combinedIndex(SI), segment = FS)).encodeByte should be(Hex.lsb("64 66 FF 20"))
      }
      "correctly represent jmp DWORD PTR fs:[bx+si] as a string" in {
        Jump(RegisterMemoryLocation.withSegmentOverride.doubleWordSize(BX.combinedIndex(SI), segment = FS)).toString should be("jmp DWORD PTR fs:[bx+si]")
      }

      "correctly encode jmp FAR 0x1000:0x2000" in {
        Jump.Far(FarPointer(0x1000.toShort, 0x2000.asInstanceOf[X86Offset.RealLongOffset])).encodeByte should be(Hex.lsb("EA 00 20 00 10"))
      }
      "correctly represent jmp FAR 0x1000:0x2000 as a string" in {
        Jump.Far(FarPointer(0x1000.toShort, 0x2000.asInstanceOf[X86Offset.RealLongOffset])).toString should be("jmp FAR 0x1000:0x2000")
      }

      "correctly encode jmp FAR 0x0030:0x200010" in {
        Jump.Far(FarPointer(0x30.toShort, 0x200010)).encodeByte should be(Hex.lsb("66 EA 10 00 20 00 30 00"))
      }
      "correctly represent jmp FAR 0x0030:0x200010 as a string" in {
        Jump.Far(FarPointer(0x30.toShort, 0x200010)).toString should be("jmp FAR 0x0030:0x00200010")
      }

      "correctly encode jmp FAR WORD PTR [bp+si]" in {
        Jump.Far(RegisterMemoryLocation.wordSize(BP.combinedIndex(SI))).encodeByte should be(Hex.lsb("FF 2A"))
      }
      "correctly represent jmp FAR WORD PTR [bp+si] as a string" in {
        Jump.Far(RegisterMemoryLocation.wordSize(BP.combinedIndex(SI))).toString should be("jmp FAR WORD PTR [bp+si]")
      }

      "correctly encode jmp FAR DWORD PTR [bp+si]" in {
        Jump.Far(RegisterMemoryLocation.doubleWordSize(BP.combinedIndex(SI))).encodeByte should be(Hex.lsb("66 FF 2A"))
      }
      "correctly represent jmp FAR DWORD PTR [bp+si] as a string" in {
        Jump.Far(RegisterMemoryLocation.doubleWordSize(BP.combinedIndex(SI))).toString should be("jmp FAR DWORD PTR [bp+si]")
      }

      "throw an AssertionError for jmp FAR QWORD PTR [bp+si]" in {
        an[AssertionError] should be thrownBy {
          Jump.Far(RegisterMemoryLocation.quadWordSize(BP.combinedIndex(SI)))
        }
      }

      "Encode a simple program with an indirect forward short jump instruction" in {
        val targetLabel = Label.unique
        val jump = Jump(targetLabel)

        val p = Section(SectionType.Text, ".test", List[Resource](
          jump,
            EncodedByteList(List.fill(1)(0x00.toByte)),
            { implicit val label: UniqueLabel =  targetLabel; EncodedByteList(List.fill(1)(0x00.toByte))}), 0)

        withClue("Jump") { jump.toInSectionState(p).encodeByte should be(Hex.lsb("EB 01")) }
      }

      "correctly represent jmp Label as a string" in {
        val targetLabel: Label = "Label"
        Jump(targetLabel).toString should be("jmp Label")
      }

      "Encode a simple program with an indirect forward conditional on count zero short jump instruction" in {
        val targetLabel = Label.unique
        val jump = JumpIfCountZero(targetLabel)

        val p = Section(SectionType.Text, ".test", List[Resource](
          jump,
          EncodedByteList(List.fill(1)(0x00.toByte)),
          { implicit val label: UniqueLabel = targetLabel; EncodedByteList(List.fill(1)(0x00.toByte))}), 0)


        jump.toInSectionState(p).size should be(2)

        withClue("Jump") { jump.toInSectionState(p).encodeByte should be(Hex.lsb("E3 01")) }
      }

      "Encode a simple program with an indirect backward short jump instruction" in {
        val targetLabel = Label.unique
        val jump = Jump(targetLabel)

        val p = Section(SectionType.Text, ".test", List[Resource](
          { implicit val label: UniqueLabel = targetLabel; EncodedByteList(List.fill(1)(0x00.toByte))},
            EncodedByteList(List.fill(1)(0x00.toByte)),
            jump), 0)

        withClue("Jump") { jump.toInSectionState(p).encodeByte should be(Hex.lsb("EB FC")) }
      }

      "Encode a simple program with an indirect backward conditional on count zero short jump instruction" in {
        val targetLabel = Label.unique
        val jump = JumpIfCountZero(targetLabel)

        val p = Section(SectionType.Text, ".test", List[Resource](
          { implicit val label: UniqueLabel = targetLabel; EncodedByteList(List.fill(1)(0x00.toByte))},
            EncodedByteList(List.fill(1)(0x00.toByte)),
            jump), 0)

        withClue("Jump") { jump.toInSectionState(p).encodeByte should be(Hex.lsb("E3 FC")) }
      }

      "Encode a simple program with an indirect forward long jump instruction" in {
        val targetLabel = Label.unique
        val jump = Jump(targetLabel)

        val p = Section(SectionType.Text, ".test", List[Resource](
          jump,
            EncodedByteList(List.fill(256)(0x00.toByte)),
            { implicit val label: UniqueLabel = targetLabel; EncodedByteList(List.fill(1)(0x00.toByte))}), 0)

        withClue("Jump") { jump.toInSectionState(p).encodeByte should be(Hex.lsb("E9 00 01")) }
      }

      "throw an AssertionError for a simple program with an indirect forward conditional on count zero long jump instruction" in {
        val targetLabel = Label.unique
        val jump = JumpIfCountZero(targetLabel)

        val p = Section(SectionType.Text, ".test", List[Resource](
          jump,
            EncodedByteList(List.fill(256)(0x00.toByte)),
            { implicit val label: UniqueLabel = targetLabel; EncodedByteList(List.fill(1)(0x00.toByte))}), 0)

        an[AssertionError] should be thrownBy { jump.toInSectionState(p).encodeByte }
      }

      "Encode a simple program with an indirect backward long jump instruction" in {
        val targetLabel = Label.unique
        val jump = Jump(targetLabel)

        val p = Section(SectionType.Text, ".test", List[Resource](
          { implicit val label: UniqueLabel = targetLabel; EncodedByteList(List.fill(1)(0x00.toByte))},
            EncodedByteList(List.fill(256)(0x00.toByte)),
            jump), 0)

        withClue("Jump") { jump.toInSectionState(p).encodeByte should be(Hex.lsb("E9 FC FE")) }
      }

      "Encode a program with two indirect short jump instructions of which one jumps across the other" in {
        val label1 = Label.unique
        val label2 = Label.unique
        val jump1 = Jump(label1)
        val jump2 = Jump(label2)

        val p = Section(SectionType.Text, ".test", List[Resource](
          { implicit val label: UniqueLabel =  label1; EncodedByteList(List.fill(1)(0x00.toByte))},
            jump2,
            EncodedByteList(List.fill(1)(0x00.toByte)),
            { implicit val label: UniqueLabel =  label2; EncodedByteList(List.fill(1)(0x00.toByte))},
            jump1), 0)

        withClue("Jump1") { jump1.toInSectionState(p).encodeByte should be(Hex.lsb("EB F9")) }
        withClue("Jump2") { jump2.toInSectionState(p).encodeByte should be(Hex.lsb("EB 01")) }
      }

      "Encode a program with two indirect short jump instructions of which one depends on the size of the other for its size" in {
        val label1 = Label.unique
        val label2 = Label.unique
        val jump1 = Jump(label1)
        val jump2 = Jump(label2)

        val p = Section(SectionType.Text, ".test", List[Resource](
          { implicit val label: UniqueLabel = label1; EncodedByteList(List.fill(1)(0x00.toByte))},
            jump2,
            EncodedByteList(List.fill(122)(0x00.toByte)),
            { implicit val label: UniqueLabel = label2; EncodedByteList(List.fill(1)(0x00.toByte))},
            jump1), 0)

        withClue("Jump1") { jump1.toInSectionState(p).encodeByte should be(Hex.lsb("EB 80")) }
        withClue("Jump2") { jump2.toInSectionState(p).encodeByte should be(Hex.lsb("EB 7A")) }
      }

      "Encode a program with two indirect jump instructions that depends on the size of the other for its size where both can be short" in {
        val label1 = Label.unique
        val label2 = Label.unique
        val jump1 = Jump(label1)
        val jump2 = Jump(label2)

        val p = Section(SectionType.Text, ".test", List[Resource](
          { implicit val label: UniqueLabel =  label1; EncodedByteList(List.fill(1)(0x00.toByte))},
            jump2,
            EncodedByteList(List.fill(123)(0x00.toByte)),
            jump1,
            EncodedByteList(List.fill(2)(0x00.toByte)),
            { implicit val label: UniqueLabel =  label2; EncodedByteList(List.fill(1)(0x00.toByte))}), 0)

        withClue("Jump1") { jump1.toInSectionState(p).encodeByte should be(Hex.lsb("EB 80")) }
        withClue("Jump2") { jump2.toInSectionState(p).encodeByte should be(Hex.lsb("EB 7F")) }
      }

      "Encode a program with two indirect jump instructions that depends on the size of the other for its size where the second forces the first to be long" in {
        val label1 = Label.unique
        val label2 = Label.unique
        val jump1 = Jump(label1)
        val jump2 = Jump(label2)

        val p = Section(SectionType.Text, ".test", List[Resource](
          { implicit val label: UniqueLabel =  label1; EncodedByteList(List.fill(1)(0x00.toByte))},
            jump2,
            EncodedByteList(List.fill(123)(0x00.toByte)),
            jump1,
            EncodedByteList(List.fill(3)(0x00.toByte)),
            { implicit val label: UniqueLabel =  label2; EncodedByteList(List.fill(1)(0x00.toByte))}), 0)

        withClue("Jump1") { jump1.toInSectionState(p).encodeByte should be(Hex.lsb("E9 7E FF")) }
        withClue("Jump2") { jump2.toInSectionState(p).encodeByte should be(Hex.lsb("E9 81 00")) }
      }

      "Encode a program with two indirect jump instructions that depends on the size of the other for its size where the first forces the second to be long" in {
        val label1 = Label.unique
        val label2 = Label.unique
        val jump1 = Jump(label1)
        val jump2 = Jump(label2)

        val p = Section(SectionType.Text, ".test", List[Resource](
          { implicit val label: UniqueLabel =  label1; EncodedByteList(List.fill(2)(0x00.toByte))},
            jump2,
            EncodedByteList(List.fill(123)(0x00.toByte)),
            jump1,
            EncodedByteList(List.fill(2)(0x00.toByte)),
            { implicit val label: UniqueLabel =  label2; EncodedByteList(List.fill(1)(0x00.toByte))}), 0)

        withClue("Jump1") { jump1.toInSectionState(p).encodeByte should be(Hex.lsb("E9 7D FF")) }
        withClue("Jump2") { jump2.toInSectionState(p).encodeByte should be(Hex.lsb("E9 80 00")) }
      }

      "Encode a program with three indirect jump instructions that depends on the size of the others for its size where all jumps can be short" in {
        val label1 = Label.unique
        val label2 = Label.unique
        val label3 = Label.unique
        val jump1 = Jump(label1)
        val jump2 = Jump(label2)
        val jump3 = Jump(label3)

        val p = Section(SectionType.Text, ".test", List[Resource](
          { implicit val label: UniqueLabel =  label1; EncodedByteList(List.fill(1)(0x00.toByte))},
            jump2,
            EncodedByteList(List.fill(60)(0x00.toByte)),
            jump3,
            EncodedByteList(List.fill(61)(0x00.toByte)),
            jump1,
            EncodedByteList(List.fill(2)(0x00.toByte)),
            { implicit val label: UniqueLabel =  label2; EncodedByteList(List.fill(62)(0x00.toByte))},
            { implicit val label: UniqueLabel =  label3; EncodedByteList(List.fill(1)(0x00.toByte))}), 0)

        withClue("Jump1") { jump1.toInSectionState(p).encodeByte should be(Hex.lsb("EB 80")) }
        withClue("Jump2") { jump2.toInSectionState(p).encodeByte should be(Hex.lsb("EB 7F")) }
        withClue("Jump3") { jump3.toInSectionState(p).encodeByte should be(Hex.lsb("EB 7F")) }
      }

      "Encode a program with three indirect jump instructions that depends on the size of the others for its size where instruction 3 forces the others to be long" in {
        val label1 = Label.unique
        val label2 = Label.unique
        val label3 = Label.unique
        val jump1 = Jump(label1)
        val jump2 = Jump(label2)
        val jump3 = Jump(label3)

        val p = Section(SectionType.Text, ".test", List[Resource](
          { implicit val label: UniqueLabel =  label1; EncodedByteList(List.fill(1)(0x00.toByte))},
            jump2,
            EncodedByteList(List.fill(60)(0x00.toByte)),
            jump3,
            EncodedByteList(List.fill(61)(0x00.toByte)),
            jump1,
            EncodedByteList(List.fill(2)(0x00.toByte)),
            { implicit val label: UniqueLabel =  label2; EncodedByteList(List.fill(63)(0x00.toByte))},
            { implicit val label: UniqueLabel =  label3; EncodedByteList(List.fill(1)(0x00.toByte))}), 0)

        withClue("Jump1") { jump1.toInSectionState(p).encodeByte should be(Hex.lsb("E9 7D FF")) }
        withClue("Jump2") { jump2.toInSectionState(p).encodeByte should be(Hex.lsb("E9 81 00")) }
        withClue("Jump3") { jump3.toInSectionState(p).encodeByte should be(Hex.lsb("E9 81 00")) }
      }

      "Encode a program with three indirect jump instructions that depends on the size of the others for its size where instruction 1 forces the others to be long" in {
        val label1 = Label.unique
        val label2 = Label.unique
        val label3 = Label.unique
        val jump1 = Jump(label1)
        val jump2 = Jump(label2)
        val jump3 = Jump(label3)

        val p = Section(SectionType.Text, ".test", List[Resource](
          { implicit val label: UniqueLabel =  label1; EncodedByteList(List.fill(2)(0x00.toByte))},
            jump2,
            EncodedByteList(List.fill(60)(0x00.toByte)),
            jump3,
            EncodedByteList(List.fill(61)(0x00.toByte)),
            jump1,
            EncodedByteList(List.fill(2)(0x00.toByte)),
            { implicit val label: UniqueLabel =  label2; EncodedByteList(List.fill(62)(0x00.toByte))},
            { implicit val label: UniqueLabel =  label3; EncodedByteList(List.fill(1)(0x00.toByte))}), 0)

        withClue("Jump1") { jump1.toInSectionState(p).encodeByte should be(Hex.lsb("E9 7C FF")) }
        withClue("Jump2") { jump2.toInSectionState(p).encodeByte should be(Hex.lsb("E9 81 00")) }
        withClue("Jump3") { jump3.toInSectionState(p).encodeByte should be(Hex.lsb("E9 80 00")) }
      }
    }

    "in protected mode" should {

      implicit val processorMode: ProcessorMode = ProcessorMode.Protected

      "correctly encode jmp 0x10" in { Jump(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("EB 10")) }
      "correctly encode ja 0x10" in { JumpIfAbove(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("77 10")) }
      "correctly encode jae 0x10" in { JumpIfAboveOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jb 0x10" in { JumpIfBelow(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode jbe 0x10" in { JumpIfBelowOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("76 10")) }
      "correctly encode jc 0x10" in { JumpIfCarry(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode je 0x10" in { JumpIfEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("74 10")) }
      "correctly encode jg 0x10" in { JumpIfGreater(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7F 10")) }
      "correctly encode jge 0x10" in { JumpIfGreaterOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7D 10")) }
      "correctly encode jl 0x10" in { JumpIfLess(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7C 10")) }
      "correctly encode jle 0x10" in { JumpIfLessOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7E 10")) }
      "correctly encode jna 0x10" in { JumpIfNotAbove(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("76 10")) }
      "correctly encode jnae 0x10" in { JumpIfNotAboveOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode jnb 0x10" in { JumpIfNotBelow(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jnbe 0x10" in { JumpIfNotBelowOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("77 10")) }
      "correctly encode jnc 0x10" in { JumpIfNoCarry(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jne 0x10" in { JumpIfNotEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("75 10")) }
      "correctly encode jng 0x10" in { JumpIfNotGreater(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7E 10")) }
      "correctly encode jnge 0x10" in { JumpIfNotGreaterOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7C 10")) }
      "correctly encode jnl 0x10" in { JumpIfNotLess(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7D 10")) }
      "correctly encode jnle 0x10" in { JumpIfNotLessOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7F 10")) }

      "correctly represent jmp 0x10 as a string" in { Jump(ShortPointer(0x10.toByte)).toString should be("jmp 0x10") }
      "correctly represent ja 0x10 as a string" in { JumpIfAbove(ShortPointer(0x10.toByte)).toString should be("ja 0x10") }
      "correctly represent jae 0x10 as a string" in { JumpIfAboveOrEqual(ShortPointer(0x10.toByte)).toString should be("jae 0x10") }
      "correctly represent jb 0x10 as a string" in { JumpIfBelow(ShortPointer(0x10.toByte)).toString should be("jb 0x10") }
      "correctly represent jbe 0x10 as a string" in { JumpIfBelowOrEqual(ShortPointer(0x10.toByte)).toString should be("jbe 0x10") }
      "correctly represent jc 0x10 as a string" in { JumpIfCarry(ShortPointer(0x10.toByte)).toString should be("jc 0x10") }
      "correctly represent je 0x10 as a string" in { JumpIfEqual(ShortPointer(0x10.toByte)).toString should be("je 0x10") }
      "correctly represent jg 0x10 as a string" in { JumpIfGreater(ShortPointer(0x10.toByte)).toString should be("jg 0x10") }
      "correctly represent jge 0x10 as a string" in { JumpIfGreaterOrEqual(ShortPointer(0x10.toByte)).toString should be("jge 0x10") }
      "correctly represent jl 0x10 as a string" in { JumpIfLess(ShortPointer(0x10.toByte)).toString should be("jl 0x10") }
      "correctly represent jle 0x10 as a string" in { JumpIfLessOrEqual(ShortPointer(0x10.toByte)).toString should be("jle 0x10") }
      "correctly represent jna 0x10 as a string" in { JumpIfNotAbove(ShortPointer(0x10.toByte)).toString should be("jna 0x10") }
      "correctly represent jnae 0x10 as a string" in { JumpIfNotAboveOrEqual(ShortPointer(0x10.toByte)).toString should be("jnae 0x10") }
      "correctly represent jnb 0x10 as a string" in { JumpIfNotBelow(ShortPointer(0x10.toByte)).toString should be("jnb 0x10") }
      "correctly represent jnbe 0x10 as a string" in { JumpIfNotBelowOrEqual(ShortPointer(0x10.toByte)).toString should be("jnbe 0x10") }
      "correctly represent jnc 0x10 as a string" in { JumpIfNoCarry(ShortPointer(0x10.toByte)).toString should be("jnc 0x10") }
      "correctly represent jne 0x10 as a string" in { JumpIfNotEqual(ShortPointer(0x10.toByte)).toString should be("jne 0x10") }
      "correctly represent jng 0x10 as a string" in { JumpIfNotGreater(ShortPointer(0x10.toByte)).toString should be("jng 0x10") }
      "correctly represent jnge 0x10 as a string" in { JumpIfNotGreaterOrEqual(ShortPointer(0x10.toByte)).toString should be("jnge 0x10") }
      "correctly represent jnl 0x10 as a string" in { JumpIfNotLess(ShortPointer(0x10.toByte)).toString should be("jnl 0x10") }
      "correctly represent jnle 0x10 as a string" in { JumpIfNotLessOrEqual(ShortPointer(0x10.toByte)).toString should be("jnle 0x10") }

      "throw an AssertionError for jmp 0x1020" in {
        an[AssertionError] should be thrownBy {
          Jump(LongPointer(0x1020.toShort)).encodeByte
        }
      }

      "throw an AssertionError for ja 0x1020" in {
        an[AssertionError] should be thrownBy {
          JumpIfAbove(LongPointer(0x1020.toShort)).encodeByte
        }
      }

      "correctly encode jmp 0x20304050" in { Jump(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("E9 50 40 30 20")) }
      "correctly encode ja 0x20304050" in { JumpIfAbove(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 87 50 40 30 20")) }
      "correctly encode jae 0x20304050" in { JumpIfAboveOrEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 83 50 40 30 20")) }
      "correctly encode jb 0x20304050" in { JumpIfBelow(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 82 50 40 30 20")) }
      "correctly encode jbe 0x20304050" in { JumpIfBelowOrEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 86 50 40 30 20")) }
      "correctly encode jc 0x20304050" in { JumpIfCarry(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 82 50 40 30 20")) }
      "correctly encode je 0x20304050" in { JumpIfEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 84 50 40 30 20")) }
      "correctly encode jg 0x20304050" in { JumpIfGreater(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 8F 50 40 30 20")) }
      "correctly encode jge 0x20304050" in { JumpIfGreaterOrEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 8D 50 40 30 20")) }
      "correctly encode jl 0x20304050" in { JumpIfLess(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 8C 50 40 30 20")) }
      "correctly encode jle 0x20304050" in { JumpIfLessOrEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 8E 50 40 30 20")) }
      "correctly encode jna 0x20304050" in { JumpIfNotAbove(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 86 50 40 30 20")) }
      "correctly encode jnae 0x20304050" in { JumpIfNotAboveOrEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 82 50 40 30 20")) }
      "correctly encode jnb 0x20304050" in { JumpIfNotBelow(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 83 50 40 30 20")) }
      "correctly encode jnbe 0x20304050" in { JumpIfNotBelowOrEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 87 50 40 30 20")) }
      "correctly encode jnc 0x20304050" in { JumpIfNoCarry(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 83 50 40 30 20")) }
      "correctly encode jne 0x20304050" in { JumpIfNotEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 85 50 40 30 20")) }
      "correctly encode jng 0x20304050" in { JumpIfNotGreater(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 8E 50 40 30 20")) }
      "correctly encode jnge 0x20304050" in { JumpIfNotGreaterOrEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 8C 50 40 30 20")) }
      "correctly encode jnl 0x20304050" in { JumpIfNotLess(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 8D 50 40 30 20")) }
      "correctly encode jnle 0x20304050" in { JumpIfNotLessOrEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 8F 50 40 30 20")) }

      "correctly encode jmp si" in {
        Jump(AX).encodeByte should be(Hex.lsb("66 FF E0"))
      }

      "correctly encode jmp [bp+si]" in {
        Jump(RegisterMemoryLocation(BP.combinedIndex(SI))).encodeByte should be(Hex.lsb("67 FF 22"))
      }

      "correctly encode jmp eax" in {
        Jump(EAX).encodeByte should be(Hex.lsb("FF E0"))
      }

      "correctly encode jmp DWORD PTR [eax]" in {
        Jump(RegisterMemoryLocation(EAX)).encodeByte should be(Hex.lsb("FF 20"))
      }

      "throw an AssertionError for jmp rax" in {
        an[AssertionError] should be thrownBy {
          Jump(RAX)
        }
      }

      "correctly encode jmp DWORD PTR fs:[bx+si]" in {
        Jump(RegisterMemoryLocation.withSegmentOverride.doubleWordSize(BX.combinedIndex(SI), segment = FS)).encodeByte should be(Hex.lsb("64 67 FF 20"))
      }

      "correctly encode jmp FAR 0x1000:0x2000" in {
        Jump.Far(FarPointer(0x1000.toShort, 0x2000.asInstanceOf[X86Offset.RealLongOffset])).encodeByte should be(Hex.lsb("66 EA 00 20 00 10"))
      }

      "correctly encode jmp FAR 0x30:0x200010" in {
        Jump.Far(FarPointer(0x30.toShort, 0x200010)).encodeByte should be(Hex.lsb("EA 10 00 20 00 30 00"))
      }

      "correctly encode jmp FAR WORD PTR [bp+si]" in {
        Jump.Far(RegisterMemoryLocation.wordSize(BP.combinedIndex(SI))).encodeByte should be(Hex.lsb("67 66 FF 2A"))
      }

      "correctly encode jmp FAR DWORD PTR [bp+si]" in {
        Jump.Far(RegisterMemoryLocation.doubleWordSize(BP.combinedIndex(SI))).encodeByte should be(Hex.lsb("67 FF 2A"))
      }

      "throw an AssertionError for jmp FAR QWORD PTR [bp+si]" in {
        an[AssertionError] should be thrownBy {
          Jump.Far(RegisterMemoryLocation.quadWordSize(BP.combinedIndex(SI)))
        }
      }

      "Encode a simple program with an indirect backward short jump instruction" in {
        val targetLabel = Label.unique
        val jump = Jump(targetLabel)

        val p = Section(SectionType.Text, ".test", List[Resource](
          { implicit val label: UniqueLabel = targetLabel; EncodedByteList(List.fill(1)(0x00.toByte))},
            EncodedByteList(List.fill(1)(0x00.toByte)),
            jump), 0)

        withClue("Jump") { jump.toInSectionState(p).encodeByte should be(Hex.lsb("EB FC")) }
      }

      "Encode a simple program with an indirect backward long jump instruction" in {
        val targetLabel = Label.unique
        val jump = Jump(targetLabel)

        val p = Section(SectionType.Text, ".test", List[Resource](
          { implicit val label: UniqueLabel = targetLabel; EncodedByteList(List.fill(1)(0x00.toByte))},
            EncodedByteList(List.fill(256)(0x00.toByte)),
            jump), 0)

        withClue("Jump") { jump.toInSectionState(p).encodeByte should be(Hex.lsb("E9 FA FE FF FF")) }
      }

      "Encode a simple program with an indirect forward long jump instruction" in {
        val targetLabel = Label.unique
        val jump = Jump(targetLabel)

        val p = Section(SectionType.Text, ".test", List[Resource](
          jump,
            EncodedByteList(List.fill(256)(0x00.toByte)),
            { implicit val label: UniqueLabel = targetLabel; EncodedByteList(List.fill(1)(0x00.toByte))}), 0)

        withClue("Jump") { jump.toInSectionState(p).encodeByte should be(Hex.lsb("E9 00 01 00 00")) }
      }

    }

    "in long mode" should {

      implicit val processorMode: ProcessorMode = ProcessorMode.Long

      "correctly encode jmp 0x10" in { Jump(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("EB 10")) }
      "correctly encode ja 0x10" in { JumpIfAbove(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("77 10")) }
      "correctly encode jae 0x10" in { JumpIfAboveOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jb 0x10" in { JumpIfBelow(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode jbe 0x10" in { JumpIfBelowOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("76 10")) }
      "correctly encode jc 0x10" in { JumpIfCarry(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode je 0x10" in { JumpIfEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("74 10")) }
      "correctly encode jg 0x10" in { JumpIfGreater(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7F 10")) }
      "correctly encode jge 0x10" in { JumpIfGreaterOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7D 10")) }
      "correctly encode jl 0x10" in { JumpIfLess(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7C 10")) }
      "correctly encode jle 0x10" in { JumpIfLessOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7E 10")) }
      "correctly encode jna 0x10" in { JumpIfNotAbove(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("76 10")) }
      "correctly encode jnae 0x10" in { JumpIfNotAboveOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode jnb 0x10" in { JumpIfNotBelow(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jnbe 0x10" in { JumpIfNotBelowOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("77 10")) }
      "correctly encode jnc 0x10" in { JumpIfNoCarry(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jne 0x10" in { JumpIfNotEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("75 10")) }
      "correctly encode jng 0x10" in { JumpIfNotGreater(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7E 10")) }
      "correctly encode jnge 0x10" in { JumpIfNotGreaterOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7C 10")) }
      "correctly encode jnl 0x10" in { JumpIfNotLess(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7D 10")) }
      "correctly encode jnle 0x10" in { JumpIfNotLessOrEqual(ShortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("7F 10")) }

      "throw an AssertionError for jmp 0x1020" in {
        an[AssertionError] should be thrownBy {
          Jump(LongPointer(0x1020.toShort)).encodeByte
        }
      }

      "throw an AssertionError for ja 0x1020" in {
        an[AssertionError] should be thrownBy {
          JumpIfAbove(LongPointer(0x1020.toShort)).encodeByte
        }
      }

      "correctly encode jmp 0x20304050" in { Jump(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("E9 50 40 30 20")) }
      "correctly encode ja 0x20304050" in { JumpIfAbove(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 87 50 40 30 20")) }
      "correctly encode jae 0x20304050" in { JumpIfAboveOrEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 83 50 40 30 20")) }
      "correctly encode jb 0x20304050" in { JumpIfBelow(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 82 50 40 30 20")) }
      "correctly encode jbe 0x20304050" in { JumpIfBelowOrEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 86 50 40 30 20")) }
      "correctly encode jc 0x20304050" in { JumpIfCarry(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 82 50 40 30 20")) }
      "correctly encode je 0x20304050" in { JumpIfEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 84 50 40 30 20")) }
      "correctly encode jg 0x20304050" in { JumpIfGreater(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 8F 50 40 30 20")) }
      "correctly encode jge 0x20304050" in { JumpIfGreaterOrEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 8D 50 40 30 20")) }
      "correctly encode jl 0x20304050" in { JumpIfLess(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 8C 50 40 30 20")) }
      "correctly encode jle 0x20304050" in { JumpIfLessOrEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 8E 50 40 30 20")) }
      "correctly encode jna 0x20304050" in { JumpIfNotAbove(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 86 50 40 30 20")) }
      "correctly encode jnae 0x20304050" in { JumpIfNotAboveOrEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 82 50 40 30 20")) }
      "correctly encode jnb 0x20304050" in { JumpIfNotBelow(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 83 50 40 30 20")) }
      "correctly encode jnbe 0x20304050" in { JumpIfNotBelowOrEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 87 50 40 30 20")) }
      "correctly encode jnc 0x20304050" in { JumpIfNoCarry(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 83 50 40 30 20")) }
      "correctly encode jne 0x20304050" in { JumpIfNotEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 85 50 40 30 20")) }
      "correctly encode jng 0x20304050" in { JumpIfNotGreater(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 8E 50 40 30 20")) }
      "correctly encode jnge 0x20304050" in { JumpIfNotGreaterOrEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 8C 50 40 30 20")) }
      "correctly encode jnl 0x20304050" in { JumpIfNotLess(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 8D 50 40 30 20")) }
      "correctly encode jnle 0x20304050" in { JumpIfNotLessOrEqual(LongPointer(0x20304050)).encodeByte should be(Hex.lsb("0F 8F 50 40 30 20")) }

      "throw an AssertionError for jmp ax" in {
        an[AssertionError] should be thrownBy {
          Jump(AX)
        }
      }

      "throw an AssertionError for jmp [bp+si]" in {
        an[AssertionError] should be thrownBy {
          Jump(RegisterMemoryLocation(BP.combinedIndex(SI))).encodeByte
        }
      }

      "throw an AssertionError for jmp eax" in {
        an[AssertionError] should be thrownBy {
          Jump(EAX)
        }
      }

      "correctly encode jmp [eax]" in {
        Jump(RegisterMemoryLocation(EAX)).encodeByte should be(Hex.lsb("67 FF 20"))
      }

      "correctly encode jmp rax" in {
        Jump(RAX).encodeByte should be(Hex.lsb("FF E0"))
      }

      "correctly encode jmp FAR 0x1000:0x2000" in {
        Jump.Far(FarPointer(0x1000.toShort, 0x2000.asInstanceOf[X86Offset.RealLongOffset])).encodeByte should be(Hex.lsb("66 EA 00 20 00 10"))
      }

      "correctly encode jmp FAR 0x30:0x200010" in {
        Jump.Far(FarPointer(0x30.toShort, 0x200010)).encodeByte should be(Hex.lsb("EA 10 00 20 00 30 00"))
      }

      "correctly encode jmp FAR WORD PTR [edx]" in {
        Jump.Far(RegisterMemoryLocation.wordSize(EDX)).encodeByte should be(Hex.lsb("67 66 FF 2A"))
      }

      "correctly encode jmp FAR DWORD PTR [edx]" in {
        Jump.Far(RegisterMemoryLocation.doubleWordSize(EDX)).encodeByte should be(Hex.lsb("67 FF 2A"))
      }

      "correctly encode jmp FAR QWORD PTR [rdx]" in {
        Jump.Far(RegisterMemoryLocation.quadWordSize(RDX)).encodeByte should be(Hex.lsb("48 FF 2A"))
      }
    }
  }
}