package assembler.arm.instructions.branch

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.arm.ProcessorMode
import assembler.arm.instructions.ARMInstruction
import assembler.arm.instructions.LoadRegister
import assembler.arm.instructions.StoreRegister
import assembler.arm.opcodes.LoadStoreAddressingTypeNormal
import assembler.arm.opcodes.LoadStoreMiscelaneousOffset
import assembler.arm.opcodes.LoadStoreOffset
import assembler.arm.opcodes.UpdateDirection
import assembler.arm.operands.Shifter
import assembler.arm.operands.registers.GeneralRegister._
import assembler.memory.MemoryPage

class LoadStoreSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[ARMInstruction])

  "an LoadRegister instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode ldr r1, [r2, #10]" in {
        LoadRegister(R1, R2, 10.toShort, LoadStoreAddressingTypeNormal.OffsetNormal).encodeByte should be(Hex.msb("e592100a"))
      }

      "correctly encode ldr r1, [r2, #-20]" in {
        LoadRegister(R1, R2, LoadStoreOffset(20.toShort, UpdateDirection.Decrement), LoadStoreAddressingTypeNormal.OffsetNormal).encodeByte should be(Hex.msb("e5121014"))
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

      "correctly encode ldr r1, [r2, #-60]!" in {
        LoadRegister(R1, R2, LoadStoreOffset(60.toShort, UpdateDirection.Decrement), LoadStoreAddressingTypeNormal.PreIndexedNormal).encodeByte should be(Hex.msb("e532103c"))
      }

      "correctly encode ldr r1, [r2, r3]" in {
        LoadRegister(R1, R2, R3, LoadStoreAddressingTypeNormal.OffsetNormal).encodeByte should be(Hex.msb("e7921003"))
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

      "correctly encode ldr r1, [r2], sl, asr #30" in {
        LoadRegister(R1, R2, Shifter.ArithmeticRightShift(R10, 30.toByte), LoadStoreAddressingTypeNormal.PostIndexedNormal).encodeByte should be(Hex.msb("e6921f4a"))
      }
    }
  }

  "an LoadRegister.byte instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode ldrb r1, [r2, #70]!" in {
        LoadRegister.byte(R1, R2, 70.toShort, LoadStoreAddressingTypeNormal.OffsetNormal).encodeByte should be(Hex.msb("e5d21046"))
      }
    }
  }

  "an LoadRegister.doubleWord instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode ldrd r1, [r2], #-40" in {
        LoadRegister.doubleWord(R1, R2, LoadStoreMiscelaneousOffset(40.toByte, UpdateDirection.Decrement), LoadStoreAddressingTypeNormal.PostIndexedNormal).encodeByte should be(Hex.msb("e04212d8"))
      }
    }
  }

  "an LoadRegister.signedByte instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode ldrsb r1, [r2, #50]!" in {
        LoadRegister.signedByte(R1, R2, 50.toByte, LoadStoreAddressingTypeNormal.PreIndexedNormal).encodeByte should be(Hex.msb("e1f213d2"))
      }
    }
  }

  "an LoadRegister.unsignedHalfWord instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode ldrh r1, [r2, #-60]!" in {
        LoadRegister.unsignedHalfWord(R1, R2, LoadStoreMiscelaneousOffset(60.toByte, UpdateDirection.Decrement), LoadStoreAddressingTypeNormal.PreIndexedNormal).encodeByte should be(Hex.msb("e17213bc"))
      }
    }
  }

  "an LoadRegister.signedHalfWord instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode ldrsh r1, [r2, r3]" in {
        LoadRegister.signedHalfWord(R1, R2, R3, LoadStoreAddressingTypeNormal.OffsetNormal).encodeByte should be(Hex.msb("e19210f3"))
      }
    }
  }

  "an LoadRegister.UserMode instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode ldrt r1, [r2], #130" in {
        LoadRegister.UserMode(R1, R2, 130.toShort).encodeByte should be(Hex.msb("e4b21082"))
      }

      "correctly encode ldrt r1, [r2], #-140" in {
        LoadRegister.UserMode(R1, R2, LoadStoreOffset(140.toShort, UpdateDirection.Decrement)).encodeByte should be(Hex.msb("e432108c"))
      }

      "correctly encode ldrt r1, [r2], r13" in {
        LoadRegister.UserMode(R1, R2, R13).encodeByte should be(Hex.msb("e6b2100d"))
      }
    }
  }

  "an LoadRegister.UserMode.byte instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode ldrbt r1, [r2], #150" in {
        LoadRegister.UserMode.byte(R1, R2, 150.toShort).encodeByte should be(Hex.msb("e4f21096"))
      }

      "correctly encode ldrbt r1, [r2], #-160" in {
        LoadRegister.UserMode.byte(R1, R2, LoadStoreOffset(160.toShort, UpdateDirection.Decrement)).encodeByte should be(Hex.msb("e47210a0"))
      }

      "correctly encode ldrbt r1, [r2], r15" in {
        LoadRegister.UserMode.byte(R1, R2, R15).encodeByte should be(Hex.msb("e6f2100f"))
      }
    }
  }

  "an StoreRegister instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode str r1, [r2, #10]" in {
        StoreRegister(R1, R2, 10.toShort, LoadStoreAddressingTypeNormal.OffsetNormal).encodeByte should be(Hex.msb("e582100a"))
      }
    }
  }

  "an StoreRegister.halfWord instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode strd r1, [r2, -r1]" in {
        StoreRegister.doubleWord(R1, R2, LoadStoreMiscelaneousOffset(R1, UpdateDirection.Decrement), LoadStoreAddressingTypeNormal.OffsetNormal).encodeByte should be(Hex.msb("e10210f1"))
      }
    }
  }

  "an StoreRegister.halfWord instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode strh r1, [r2, #40]!" in {
        StoreRegister.halfWord(R1, R2, LoadStoreMiscelaneousOffset(40.toByte, UpdateDirection.Increment), LoadStoreAddressingTypeNormal.PreIndexedNormal).encodeByte should be(Hex.msb("e1e212b8"))
      }
    }
  }
}