package assembler.arm.instructions

import assembler._
import assembler.arm.ProcessorMode
import assembler.arm.operands.registers.GeneralRegister._
import assembler.arm.operands.{Condition, Shifter}
import assembler.arm.operations._
import assembler.output.raw.Raw
import assembler.resource.Resource
import assembler.sections.Section
import org.scalatest.{Matchers, WordSpec}

class LoadStoreSuite extends WordSpec with Matchers {

  "an LoadRegister instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode ldr r1, [r2, #10]" in {
        LoadRegister(R1, R2, 10.toShort, LoadStoreAddressingTypeNormal.OffsetNormal).encodeByte should be(Hex.msb("e592100a"))
      }

      "correctly represent ldr r1, [r2, #10] as a string" in {
        LoadRegister(R1, R2, 10.toShort, LoadStoreAddressingTypeNormal.OffsetNormal).toString should be("ldr r1, [r2, #10]")
      }

      "correctly encode ldr r1, [r2], #-20" in {
        LoadRegister(R1, R2, (-20).toShort, LoadStoreAddressingTypeNormal.PostIndexedNormal).encodeByte should be(Hex.msb("e4121014"))
      }

      "correctly represent ldr r1, [r2], #-20 as a string" in {
        LoadRegister(R1, R2, (-20).toShort, LoadStoreAddressingTypeNormal.PostIndexedNormal).toString should be("ldr r1, [r2], #-20")
      }

      "correctly encode ldr r1, [r2] #30" in {
        LoadRegister(R1, R2, 30.toShort, LoadStoreAddressingTypeNormal.PostIndexedNormal).encodeByte should be(Hex.msb("e492101e"))
      }

      "correctly encode ldr r1, [r2] #-40" in {
        LoadRegister(R1, R2, LoadStoreOffset(40.toShort, UpdateDirection.Decrement), LoadStoreAddressingTypeNormal.PostIndexedNormal).encodeByte should be(Hex.msb("e4121028"))
      }

      "correctly encode ldr r1, [r2, #50]!" in {
        LoadRegister(R1, R2, 50.toShort, LoadStoreAddressingTypeNormal.PreIndexedNormal).encodeByte should be(Hex.msb("e5b21032"))
      }

      "correctly represent ldr r1, [r2, #50]! as a string" in {
        LoadRegister(R1, R2, 50.toShort, LoadStoreAddressingTypeNormal.PreIndexedNormal).toString should be("ldr r1, [r2, #50]!")
      }

      "correctly encode ldr r1, [r2, #-60]!" in {
        LoadRegister(R1, R2, (-60).toShort, LoadStoreAddressingTypeNormal.PreIndexedNormal).encodeByte should be(Hex.msb("e532103c"))
      }

      "correctly encode ldr r1, [r2, r3]" in {
        LoadRegister(R1, R2, R3, LoadStoreAddressingTypeNormal.OffsetNormal).encodeByte should be(Hex.msb("e7921003"))
      }

      "correctly represent ldr r1, [r2, r3] as a string" in {
        LoadRegister(R1, R2, R3, LoadStoreAddressingTypeNormal.OffsetNormal).toString should be("ldr r1, [r2, r3]")
      }

      "correctly encode ldr r1, [r2, -r8]!" in {
        LoadRegister(R1, R2, LoadStoreOffset(R8, UpdateDirection.Decrement), LoadStoreAddressingTypeNormal.PreIndexedNormal).encodeByte should be(Hex.msb("e7321008"))
      }

      "correctly encode ldr r1, [r2, r3, lsl #10]" in {
        LoadRegister(R1, R2, Shifter.LogicalLeftShift(R3, 10.toByte), LoadStoreAddressingTypeNormal.OffsetNormal).encodeByte should be(Hex.msb("e7921503"))
      }

      "correctly encode ldr r1, [r2, -r9, lsr #20]" in {
        LoadRegister(R1, R2, LoadStoreOffset(Shifter.LogicalRightShift(R9, 20.toByte), UpdateDirection.Decrement), LoadStoreAddressingTypeNormal.OffsetNormal).encodeByte should be(Hex.msb("e7121a29"))
      }

      "correctly encode ldr r1, [r2], r10, asr #30" in {
        LoadRegister(R1, R2, Shifter.ArithmeticRightShift(R10, 30.toByte), LoadStoreAddressingTypeNormal.PostIndexedNormal).encodeByte should be(Hex.msb("e6921f4a"))
      }

      "correctly represent ldr r1, [r2], r10, asr #30 as a string" in {
        LoadRegister(R1, R2, Shifter.ArithmeticRightShift(R10, 30.toByte), LoadStoreAddressingTypeNormal.PostIndexedNormal).toString should be("ldr r1, [r2], r10, asr #30")
      }

      "correctly encode a indirect ldr instruction with an indirect reference to a labeled resource" in {
        val targetLabel = Label.unique
        val reference = LoadRegister(targetLabel, R1)
        val p = Section.text(List[Resource](
          reference,
          EncodedBytes(List.fill(4)(0x00.toByte)),
          EncodedString("Test").label(targetLabel)
        ))

        val application = Raw(p, 0)
        application.encodablesForDependencies(Seq(reference))(reference).encodeByte should be(Hex.msb("e59f1000"))
      }

      "correctly encode a conditional indirect ldr instruction with an indirect reference to a labeled resource" in {
        val targetLabel = Label.unique
        val reference = LoadRegister(targetLabel, R1, Condition.CarrySet)
        val p = Section.text(List[Resource](
          EncodedString("Test").label(targetLabel),
          EncodedBytes(List.fill(4)(0x00.toByte)),
          reference
        ))

        val application = Raw(p, 0)
        application.encodablesForDependencies(Seq(reference))(reference).encodeByte should be(Hex.msb("251F1010"))
      }
    }
  }

  "an LoadRegister.byte instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode ldrb r1, [r2, #70]" in {
        LoadRegister.byte(R1, R2, 70.toShort, LoadStoreAddressingTypeNormal.OffsetNormal).encodeByte should be(Hex.msb("e5d21046"))
      }

      "correctly represent ldrb r1, [r2, #70] as a string" in {
        LoadRegister.byte(R1, R2, 70.toShort, LoadStoreAddressingTypeNormal.OffsetNormal).toString should be("ldrb r1, [r2, #70]")
      }
    }
  }

  "an LoadRegister.doubleWord instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode ldrd r1, [r2], #-40" in {
        LoadRegister.doubleWord(R1, R2, (-40).toByte, LoadStoreAddressingTypeNormal.PostIndexedNormal).encodeByte should be(Hex.msb("e04212d8"))
      }

      "correctly represent ldrd r1, [r2], #-40 as a string" in {
        LoadRegister.doubleWord(R1, R2, (-40).toByte, LoadStoreAddressingTypeNormal.PostIndexedNormal).toString should be("ldrd r1, [r2], #-40")
      }
    }
  }

  "an LoadRegister.signedByte instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode ldrsb r1, [r2, #50]!" in {
        LoadRegister.signedByte(R1, R2, 50.toByte, LoadStoreAddressingTypeNormal.PreIndexedNormal).encodeByte should be(Hex.msb("e1f213d2"))
      }

      "correctly represent ldrsb r1, [r2, #50]! as a string" in {
        LoadRegister.signedByte(R1, R2, 50.toByte, LoadStoreAddressingTypeNormal.PreIndexedNormal).toString should be("ldrsb r1, [r2, #50]!")
      }
    }
  }

  "an LoadRegister.unsignedHalfWord instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode ldrh r1, [r2, #-60]!" in {
        LoadRegister.unsignedHalfWord(R1, R2, (-60).toByte, LoadStoreAddressingTypeNormal.PreIndexedNormal).encodeByte should be(Hex.msb("e17213bc"))
      }

      "correctly represent ldrh r1, [r2, #-60]! as a string" in {
        LoadRegister.unsignedHalfWord(R1, R2, (-60).toByte, LoadStoreAddressingTypeNormal.PreIndexedNormal).toString should be("ldrh r1, [r2, #-60]!")
      }
    }
  }

  "an LoadRegister.signedHalfWord instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode ldrsh r1, [r2, r3]" in {
        LoadRegister.signedHalfWord(R1, R2, R3, LoadStoreAddressingTypeNormal.OffsetNormal).encodeByte should be(Hex.msb("e19210f3"))
      }

      "correctly represent ldrsh r1, [r2, r3] as a string" in {
        LoadRegister.signedHalfWord(R1, R2, R3, LoadStoreAddressingTypeNormal.OffsetNormal).toString should be("ldrsh r1, [r2, r3]")
      }
    }
  }

  "an LoadRegister.UserMode instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode ldrt r1, [r2], #130" in {
        LoadRegister.UserMode(R1, R2, 130.toShort).encodeByte should be(Hex.msb("e4b21082"))
      }

      "correctly represent ldrt r1, [r2], #130 as a string" in {
        LoadRegister.UserMode(R1, R2, 130.toShort).toString should be("ldrt r1, [r2], #130")
      }

      "correctly encode ldrt r1, [r2], #-140" in {
        LoadRegister.UserMode(R1, R2, (-140).toShort).encodeByte should be(Hex.msb("e432108c"))
      }

      "correctly encode ldrt r1, [r2], r13" in {
        LoadRegister.UserMode(R1, R2, R13).encodeByte should be(Hex.msb("e6b2100d"))
      }
    }
  }

  "an LoadRegister.UserMode.byte instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode ldrbt r1, [r2], #150" in {
        LoadRegister.UserMode.byte(R1, R2, 150.toShort).encodeByte should be(Hex.msb("e4f21096"))
      }

      "correctly encode ldrbt r1, [r2], #-160" in {
        LoadRegister.UserMode.byte(R1, R2, (-160).toShort).encodeByte should be(Hex.msb("e47210a0"))
      }

      "correctly encode ldrbt r1, [r2], r15" in {
        LoadRegister.UserMode.byte(R1, R2, R15).encodeByte should be(Hex.msb("e6f2100f"))
      }
    }
  }

  "an StoreRegister instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode str r1, [r2, #10]" in {
        StoreRegister(R1, R2, 10.toShort, LoadStoreAddressingTypeNormal.OffsetNormal).encodeByte should be(Hex.msb("e582100a"))
      }
    }
  }

  "an StoreRegister.halfWord instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode strd r1, [r2, -r1]" in {
        StoreRegister.doubleWord(R1, R2, LoadStoreMiscellaneousOffset(R1, UpdateDirection.Decrement), LoadStoreAddressingTypeNormal.OffsetNormal).encodeByte should be(Hex.msb("e10210f1"))
      }
    }
  }

  "an StoreRegister.halfWord instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode strh r1, [r2, #40]!" in {
        StoreRegister.halfWord(R1, R2, 40.toByte, LoadStoreAddressingTypeNormal.PreIndexedNormal).encodeByte should be(Hex.msb("e1e212b8"))
      }
    }
  }
}