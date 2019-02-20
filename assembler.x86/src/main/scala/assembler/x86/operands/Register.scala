package assembler.x86.operands

import assembler.x86.{ProcessorMode, RexRequirement}

sealed abstract class Register extends Operand

sealed abstract class GeneralPurposeRegister(val registerCode: Byte, val mnemonic: String)
  extends Register with ModRMEncodableOperand with ValueSize {
  val modValue: Byte = 0x03.toByte
  val registerOrMemoryModeCode: Byte = registerCode
}

sealed abstract class GeneralPurposeRexRegister(registerCode: Byte, mnemonic: String)
  extends GeneralPurposeRegister(registerCode, mnemonic) {
  override def isValidForMode(processorMode: ProcessorMode): Boolean = processorMode == ProcessorMode.Long
  override def rexRequirements(rexRequirement: RexRequirement): Set[RexRequirement] = Set(rexRequirement)
}

sealed trait RegisterReference {
  val defaultSegment: SegmentRegister = Segment.Data
  val indexCode: Byte

  def onlyWithDisplacement: Boolean = false
}

sealed trait IndexRegister extends RegisterReference {
  self: GeneralPurposeRegister =>
}

sealed trait RealModeIndexRegister extends IndexRegister {
  self: GeneralPurposeRegister =>
}

sealed trait CombinableRealModeIndexRegister extends RealModeIndexRegister {
  self: GeneralPurposeRegister =>

  def +(base: BaseRegisterReference): BaseIndexReference =
    base.combinedIndex(this)
}

sealed trait BaseRegisterReference extends ModRMEncodableOperand {
  self: GeneralPurposeRegister =>

  def combinedIndex(index: CombinableRealModeIndexRegister): BaseIndexReference

  final def +(index: CombinableRealModeIndexRegister): BaseIndexReference =
    combinedIndex(index)
}

sealed trait ProtectedModeIndexRegister extends IndexRegister {
  self: GeneralPurposeRegister =>
  override val indexCode: Byte = self.registerOrMemoryModeCode
}

sealed trait SIBIndexRegister extends ModRMEncodableOperand {
  val defaultSIBSegment: SegmentRegister = Segment.Data
  val SIBIndexCode: Byte = registerOrMemoryModeCode
}

sealed trait SIBBaseRegister extends ModRMEncodableOperand {
  val SIBBaseCode: Byte = registerOrMemoryModeCode
}

sealed trait ByteRegister extends GeneralPurposeRegister with ByteSize

sealed trait LowByteRegister extends ByteRegister {
  override def toString: String = if ( mnemonic.startsWith("r")) s"${mnemonic}l" else mnemonic.replace('x', 'l')
}

sealed trait HighByteRegister extends ByteRegister {
  override val registerOrMemoryModeCode: Byte = (registerCode + 0x04).toByte

  override def toString: String = mnemonic.replace('x', 'h')
}

sealed trait WordRegister extends GeneralPurposeRegister with WordSize {
  override def toString: String = if (mnemonic.startsWith("r")) s"${mnemonic}w" else mnemonic
}

sealed trait DoubleWordRegister extends GeneralPurposeRegister with SIBIndexRegister with SIBBaseRegister with DoubleWordSize {
  override def toString: String = if (mnemonic.startsWith("r")) s"${mnemonic}d" else s"e$mnemonic"
}

sealed trait QuadWordRegister extends GeneralPurposeRegister with SIBIndexRegister with SIBBaseRegister with QuadWordSize {
  override def toString: String = if (mnemonic.startsWith("r")) mnemonic else s"r$mnemonic"
}

sealed abstract class AccumulatorRegister extends GeneralPurposeRegister(0x00, "ax")

object Accumulator {
  case object LowByte extends AccumulatorRegister with LowByteRegister
  case object HighByte extends AccumulatorRegister with HighByteRegister
  case object Word extends AccumulatorRegister with WordRegister
  case object DoubleWord extends AccumulatorRegister with DoubleWordRegister with ProtectedModeIndexRegister
  case object QuadWord extends AccumulatorRegister with QuadWordRegister with ProtectedModeIndexRegister
}

sealed abstract class CountRegister extends GeneralPurposeRegister(0x01, "cx")

object Count {
  case object LowByte extends CountRegister with LowByteRegister
  case object HighByte extends CountRegister with HighByteRegister
  case object Word extends CountRegister with WordRegister
  case object DoubleWord extends CountRegister with DoubleWordRegister with ProtectedModeIndexRegister
  case object QuadWord extends CountRegister with QuadWordRegister with ProtectedModeIndexRegister
}

sealed abstract class DataRegister extends GeneralPurposeRegister(0x02, "dx")

object Data {
  case object LowByte extends DataRegister with LowByteRegister
  case object HighByte extends DataRegister with HighByteRegister
  case object Word extends DataRegister with WordRegister
  case object DoubleWord extends DataRegister with DoubleWordRegister with ProtectedModeIndexRegister
  case object QuadWord extends DataRegister with QuadWordRegister with ProtectedModeIndexRegister
}

sealed abstract class BaseRegister extends GeneralPurposeRegister(0x03, "bx")

object Base {
  case object LowByte extends BaseRegister with LowByteRegister
  case object HighByte extends BaseRegister with HighByteRegister
  case object Word extends BaseRegister with WordRegister with BaseRegisterReference with RealModeIndexRegister {
    override val indexCode: Byte = 0x07.toByte

    override def combinedIndex(index: CombinableRealModeIndexRegister): BaseIndexReference =
      index match {
        case SourceIndex.Real => BaseIndexReference.BX_SI
        case DestinationIndex.Real => BaseIndexReference.BX_DI
      }
  }
  case object DoubleWord extends BaseRegister with DoubleWordRegister with ProtectedModeIndexRegister
  case object QuadWord extends BaseRegister with QuadWordRegister with ProtectedModeIndexRegister
}

sealed abstract class SegmentRegister(val registerCode: Byte, val mnemonic: String) extends Register {
  override def toString: String = mnemonic
}

object Segment {
  case object Extra extends SegmentRegister(0x00, "es")
  case object Code extends SegmentRegister(0x01, "cs")
  case object Stack extends SegmentRegister(0x02, "ss")
  case object Data extends SegmentRegister(0x03, "ds")
  case object MoreExtra extends SegmentRegister(0x04, "fs")
  case object StillMoreExtra extends SegmentRegister(0x05, "gs")
}

sealed abstract class SourcePointer extends GeneralPurposeRegister(0x04, "sp")

object SourcePointer {
  case object Real extends SourcePointer with WordRegister
  case object Protected extends SourcePointer with DoubleWordRegister
  case object Long extends SourcePointer with QuadWordRegister
  case object LongLowByte extends SourcePointer with LowByteRegister
}

sealed abstract class BasePointer extends GeneralPurposeRegister(0x05, "bp")

object BasePointer {
  case object Real extends BasePointer with WordRegister with BaseRegisterReference with RealModeIndexRegister {
    override val indexCode: Byte = 0x06.toByte

    override val onlyWithDisplacement: Boolean = true

    override def combinedIndex(index: CombinableRealModeIndexRegister): BaseIndexReference =
      index match {
        case SourceIndex.Real => BaseIndexReference.BP_SI
        case DestinationIndex.Real => BaseIndexReference.BP_DI
      }
  }
  case object Protected extends BasePointer with DoubleWordRegister with ProtectedModeIndexRegister {
    override val onlyWithDisplacement: Boolean = true
  }

  case object Long extends BasePointer with QuadWordRegister with ProtectedModeIndexRegister

  case object LongLowByte extends BasePointer with LowByteRegister
}

sealed abstract class SourceIndex extends GeneralPurposeRegister(0x06, "si")

object SourceIndex {

  final case object Real extends SourceIndex with WordRegister with CombinableRealModeIndexRegister {
    override val indexCode: Byte = 0x04.toByte
  }

  case object Protected extends SourceIndex with DoubleWordRegister with ProtectedModeIndexRegister {
    override val defaultSegment: SegmentRegister = Segment.Extra
  }

  case object Long extends SourceIndex with QuadWordRegister with ProtectedModeIndexRegister {
    override val defaultSegment: SegmentRegister = Segment.Extra
  }

  case object LongLowByte extends SourceIndex with LowByteRegister
}

sealed abstract class DestinationIndex extends GeneralPurposeRegister(0x07, "di")

object DestinationIndex {
  final case object Real extends DestinationIndex with WordRegister with CombinableRealModeIndexRegister {
    override val defaultSegment: SegmentRegister = Segment.Extra
    override val indexCode: Byte = 0x05.toByte
  }

  case object Protected extends DestinationIndex with DoubleWordRegister with ProtectedModeIndexRegister

  case object Long extends DestinationIndex with QuadWordRegister with ProtectedModeIndexRegister

  case object LongLowByte extends DestinationIndex with LowByteRegister
}

sealed abstract class Rex8 extends GeneralPurposeRexRegister(0x00, "r8")

object Register8 {
  case object LowByte extends Rex8 with LowByteRegister
  case object Word extends Rex8 with WordRegister
  case object DoubleWord extends Rex8 with DoubleWordRegister with ProtectedModeIndexRegister
  case object QuadWord extends Rex8 with QuadWordRegister with ProtectedModeIndexRegister
}

sealed abstract class Rex9 extends GeneralPurposeRexRegister(0x01, "r9")

object Register9 {
  case object LowByte extends Rex9 with LowByteRegister
  case object Word extends Rex9 with WordRegister
  case object DoubleWord extends Rex9 with DoubleWordRegister with ProtectedModeIndexRegister
  case object QuadWord extends Rex9 with QuadWordRegister with ProtectedModeIndexRegister
}

sealed abstract class Rex10 extends GeneralPurposeRexRegister(0x02, "r10")

object Register10 {
  case object LowByte extends Rex10 with LowByteRegister
  case object Word extends Rex10 with WordRegister
  case object DoubleWord extends Rex10 with DoubleWordRegister with ProtectedModeIndexRegister
  case object QuadWord extends Rex10 with QuadWordRegister with ProtectedModeIndexRegister
}

sealed abstract class Rex11 extends GeneralPurposeRexRegister(0x03, "r11")

object Register11 {
  case object LowByte extends Rex11 with LowByteRegister
  case object Word extends Rex11 with WordRegister
  case object DoubleWord extends Rex11 with DoubleWordRegister with ProtectedModeIndexRegister
  case object QuadWord extends Rex11 with QuadWordRegister with ProtectedModeIndexRegister
}

sealed abstract class Rex12 extends GeneralPurposeRexRegister(0x04, "r12")

object Register12 {
  case object LowByte extends Rex12 with LowByteRegister
  case object Word extends Rex12 with WordRegister
  case object DoubleWord extends Rex12 with DoubleWordRegister
  case object QuadWord extends Rex12 with QuadWordRegister
}

sealed abstract class Rex13 extends GeneralPurposeRexRegister(0x05, "r13")

object Register13 {
  case object LowByte extends Rex13 with LowByteRegister
  case object Word extends Rex13 with WordRegister
  case object DoubleWord extends Rex13 with DoubleWordRegister with ProtectedModeIndexRegister
  case object QuadWord extends Rex13 with QuadWordRegister with ProtectedModeIndexRegister
}

sealed abstract class Rex14 extends GeneralPurposeRexRegister(0x06, "r14")

object Register14 {
  case object LowByte extends Rex14 with LowByteRegister
  case object Word extends Rex14 with WordRegister
  case object DoubleWord extends Rex14 with DoubleWordRegister with ProtectedModeIndexRegister
  case object QuadWord extends Rex14 with QuadWordRegister with ProtectedModeIndexRegister
}

sealed abstract class Rex15 extends GeneralPurposeRexRegister(0x07, "r15")

object Register15 {
  case object LowByte extends Rex15 with LowByteRegister
  case object Word extends Rex15 with WordRegister
  case object DoubleWord extends Rex15 with DoubleWordRegister with ProtectedModeIndexRegister
  case object QuadWord extends Rex15 with QuadWordRegister with ProtectedModeIndexRegister
}

sealed abstract class BaseIndexReference(
  val base: GeneralPurposeRegister with BaseRegisterReference,
  val index: GeneralPurposeRegister with CombinableRealModeIndexRegister,
  override val indexCode: Byte)
  extends RegisterReference {

  override val defaultSegment: SegmentRegister = index.defaultSegment

  override def toString = s"$base+$index"
}

object BaseIndexReference {
  object BX_SI extends BaseIndexReference(Base.Word, SourceIndex.Real, 0x00)
  object BX_DI extends BaseIndexReference(Base.Word, DestinationIndex.Real, 0x01)
  object BP_SI extends BaseIndexReference(BasePointer.Real, SourceIndex.Real, 0x02)
  object BP_DI extends BaseIndexReference(BasePointer.Real, DestinationIndex.Real, 0x03)
}

object Register {
  trait I8086Registers {
    val AL: Accumulator.LowByte.type = Accumulator.LowByte
    val CL: Count.LowByte.type = Count.LowByte
    val DL: Data.LowByte.type = Data.LowByte
    val BL: Base.LowByte.type = Base.LowByte

    val AH: Accumulator.HighByte.type = Accumulator.HighByte
    val CH: Count.HighByte.type = Count.HighByte
    val DH: Data.HighByte.type = Data.HighByte
    val BH: Base.HighByte.type = Base.HighByte

    val AX: Accumulator.Word.type = Accumulator.Word
    val CX: Count.Word.type = Count.Word
    val DX: Data.Word.type = Data.Word
    val BX: Base.Word.type = Base.Word

    val ES: SegmentRegister = Segment.Extra
    val CS: SegmentRegister = Segment.Code
    val SS: SegmentRegister = Segment.Stack
    val DS: SegmentRegister = Segment.Data
    val FS: SegmentRegister = Segment.MoreExtra
    val GS: SegmentRegister = Segment.StillMoreExtra

    val SP: SourcePointer.Real.type = SourcePointer.Real
    val BP: BasePointer.Real.type = BasePointer.Real

    val SI: SourceIndex.Real.type = SourceIndex.Real
    val DI: DestinationIndex.Real.type = DestinationIndex.Real
  }

  trait I386Registers {
    val EAX: Accumulator.DoubleWord.type = Accumulator.DoubleWord
    val ECX: Count.DoubleWord.type = Count.DoubleWord
    val EDX: Data.DoubleWord.type = Data.DoubleWord
    val EBX: Base.DoubleWord.type = Base.DoubleWord

    val ESP: SourcePointer.Protected.type = SourcePointer.Protected
    val EBP: BasePointer.Protected.type = BasePointer.Protected

    val ESI: SourceIndex.Protected.type = SourceIndex.Protected
    val EDI: DestinationIndex.Protected.type = DestinationIndex.Protected
  }

  trait X64Registers {
    val RAX: Accumulator.QuadWord.type = Accumulator.QuadWord
    val RCX: Count.QuadWord.type = Count.QuadWord
    val RDX: Data.QuadWord.type = Data.QuadWord
    val RBX: Base.QuadWord.type = Base.QuadWord

    val RSP: SourcePointer.Long.type = SourcePointer.Long
    val RBP: BasePointer.Long.type = BasePointer.Long

    val SPL: SourcePointer.LongLowByte.type = SourcePointer.LongLowByte
    val BPL: BasePointer.LongLowByte.type = BasePointer.LongLowByte

    val RSI: SourceIndex.Long.type = SourceIndex.Long
    val RDI: DestinationIndex.Long.type = DestinationIndex.Long

    val SIL: SourceIndex.LongLowByte.type = SourceIndex.LongLowByte
    val DIL: DestinationIndex.LongLowByte.type = DestinationIndex.LongLowByte

    val R8L: Register8.LowByte.type = Register8.LowByte
    val R9L: Register9.LowByte.type = Register9.LowByte
    val R10L: Register10.LowByte.type = Register10.LowByte
    val R11L: Register11.LowByte.type = Register11.LowByte
    val R12L: Register12.LowByte.type = Register12.LowByte
    val R13L: Register13.LowByte.type = Register13.LowByte
    val R14L: Register14.LowByte.type = Register14.LowByte
    val R15L: Register15.LowByte.type = Register15.LowByte

    val R8W: Register8.Word.type = Register8.Word
    val R9W: Register9.Word.type = Register9.Word
    val R10W: Register10.Word.type = Register10.Word
    val R11W: Register11.Word.type = Register11.Word
    val R12W: Register12.Word.type = Register12.Word
    val R13W: Register13.Word.type = Register13.Word
    val R14W: Register14.Word.type = Register14.Word
    val R15W: Register15.Word.type = Register15.Word

    val R8D: Register8.DoubleWord.type = Register8.DoubleWord
    val R9D: Register9.DoubleWord.type = Register9.DoubleWord
    val R10D: Register10.DoubleWord.type = Register10.DoubleWord
    val R11D: Register11.DoubleWord.type = Register11.DoubleWord
    val R12D: Register12.DoubleWord.type = Register12.DoubleWord
    val R13D: Register13.DoubleWord.type = Register13.DoubleWord
    val R14D: Register14.DoubleWord.type = Register14.DoubleWord
    val R15D: Register15.DoubleWord.type = Register15.DoubleWord

    val R8: Register8.QuadWord.type = Register8.QuadWord
    val R9: Register9.QuadWord.type = Register9.QuadWord
    val R10: Register10.QuadWord.type = Register10.QuadWord
    val R11: Register11.QuadWord.type = Register11.QuadWord
    val R12: Register12.QuadWord.type = Register12.QuadWord
    val R13: Register13.QuadWord.type = Register13.QuadWord
    val R14: Register14.QuadWord.type = Register14.QuadWord
    val R15: Register15.QuadWord.type = Register15.QuadWord

  }

}