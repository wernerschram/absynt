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

sealed abstract class AccumulatorRegister extends GeneralPurposeRegister(0x00, "ax")
sealed abstract class CountRegister extends GeneralPurposeRegister(0x01, "cx")
sealed abstract class DataRegister extends GeneralPurposeRegister(0x02, "dx")
sealed abstract class BaseRegister extends GeneralPurposeRegister(0x03, "bx")
sealed abstract class SourcePointer extends GeneralPurposeRegister(0x04, "sp")
sealed abstract class BasePointer extends GeneralPurposeRegister(0x05, "bp")
sealed abstract class SourceIndex extends GeneralPurposeRegister(0x06, "si")
sealed abstract class DestinationIndex extends GeneralPurposeRegister(0x07, "di")

sealed abstract class Rex8 extends GeneralPurposeRexRegister(0x00, "r8")
sealed abstract class Rex9 extends GeneralPurposeRexRegister(0x01, "r9")
sealed abstract class Rex10 extends GeneralPurposeRexRegister(0x02, "r10")
sealed abstract class Rex11 extends GeneralPurposeRexRegister(0x03, "r11")
sealed abstract class Rex12 extends GeneralPurposeRexRegister(0x04, "r12")
sealed abstract class Rex13 extends GeneralPurposeRexRegister(0x05, "r13")
sealed abstract class Rex14 extends GeneralPurposeRexRegister(0x06, "r14")
sealed abstract class Rex15 extends GeneralPurposeRexRegister(0x07, "r15")

sealed abstract class SegmentRegister(val registerCode: Byte, val mnemonic: String) extends Register {
  override def toString: String = mnemonic
}

sealed trait RegisterReference {
  val defaultSegment: SegmentRegister = Register.Segment.Data
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

sealed abstract class BaseIndexReference(
  val base: GeneralPurposeRegister with BaseRegisterReference,
  val index: GeneralPurposeRegister with CombinableRealModeIndexRegister,
  override val indexCode: Byte)
  extends RegisterReference {

  override val defaultSegment: SegmentRegister = index.defaultSegment

  override def toString = s"$base+$index"
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
  val defaultSIBSegment: SegmentRegister = Register.Segment.Data
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

object Register {

  object Accumulator {
    case object LowByte extends AccumulatorRegister with LowByteRegister
    case object HighByte extends AccumulatorRegister with HighByteRegister
    case object Word extends AccumulatorRegister with WordRegister
    case object DoubleWord extends AccumulatorRegister with DoubleWordRegister with ProtectedModeIndexRegister
    case object QuadWord extends AccumulatorRegister with QuadWordRegister with ProtectedModeIndexRegister
  }

  object Base {
    case object LowByte extends BaseRegister with LowByteRegister
    case object HighByte extends BaseRegister with HighByteRegister
    case object Word extends BaseRegister with WordRegister with BaseRegisterReference with RealModeIndexRegister {
      override val indexCode: Byte = 0x07.toByte

      override def combinedIndex(index: CombinableRealModeIndexRegister): BaseIndexReference =
        index match {
          case Index.RealSource => BaseIndexReference.BX_SI
          case Index.RealDestination => BaseIndexReference.BX_DI
        }
    }
    case object DoubleWord extends BaseRegister with DoubleWordRegister with ProtectedModeIndexRegister
    case object QuadWord extends BaseRegister with QuadWordRegister with ProtectedModeIndexRegister
  }

  object Count {
    case object LowByte extends CountRegister with LowByteRegister
    case object HighByte extends CountRegister with HighByteRegister
    case object Word extends CountRegister with WordRegister
    case object DoubleWord extends CountRegister with DoubleWordRegister with ProtectedModeIndexRegister
    case object QuadWord extends CountRegister with QuadWordRegister with ProtectedModeIndexRegister
  }

  object Data {
    case object LowByte extends DataRegister with LowByteRegister
    case object HighByte extends DataRegister with HighByteRegister
    case object Word extends DataRegister with WordRegister
    case object DoubleWord extends DataRegister with DoubleWordRegister with ProtectedModeIndexRegister
    case object QuadWord extends DataRegister with QuadWordRegister with ProtectedModeIndexRegister
  }

  object Segment {
    case object Extra extends SegmentRegister(0x00, "es")
    case object Code extends SegmentRegister(0x01, "cs")
    case object Stack extends SegmentRegister(0x02, "ss")
    case object Data extends SegmentRegister(0x03, "ds")
    case object MoreExtra extends SegmentRegister(0x04, "fs")
    case object StillMoreExtra extends SegmentRegister(0x05, "gs")
  }

  object Pointer {
    case object RealSource extends SourcePointer with WordRegister
    case object RealBase extends BasePointer with WordRegister with BaseRegisterReference with RealModeIndexRegister {
      override val indexCode: Byte = 0x06.toByte

      override val onlyWithDisplacement: Boolean = true

      override def combinedIndex(index: CombinableRealModeIndexRegister): BaseIndexReference =
        index match {
          case Index.RealSource => BaseIndexReference.BP_SI
          case Index.RealDestination => BaseIndexReference.BP_DI
        }
    }
    case object ProtectedSource extends SourcePointer with DoubleWordRegister
    case object ProtectedBase extends BasePointer with DoubleWordRegister with ProtectedModeIndexRegister {
      override val onlyWithDisplacement: Boolean = true
    }

    case object LongSource extends SourcePointer with QuadWordRegister
    case object LongBase extends BasePointer with QuadWordRegister with ProtectedModeIndexRegister

    case object LongSourceLowByte extends SourcePointer with LowByteRegister
    case object LongBaseLowByte extends BasePointer with LowByteRegister
  }

  object Index {
    final case object RealSource extends SourceIndex with WordRegister with CombinableRealModeIndexRegister {
      override val indexCode: Byte = 0x04.toByte
    }

    final case object RealDestination extends DestinationIndex with WordRegister with CombinableRealModeIndexRegister {
      override val defaultSegment: SegmentRegister = Register.Segment.Extra
      override val indexCode: Byte = 0x05.toByte
    }

    case object ProtectedSource extends SourceIndex with DoubleWordRegister with ProtectedModeIndexRegister {
      override val defaultSegment: SegmentRegister = Register.Segment.Extra
    }
    case object ProtectedDestination extends DestinationIndex with DoubleWordRegister with ProtectedModeIndexRegister

    // x86-64 registers
    case object LongSource extends SourceIndex with QuadWordRegister with ProtectedModeIndexRegister {
      override val defaultSegment: SegmentRegister = Register.Segment.Extra
    }
    case object LongDestination extends DestinationIndex with QuadWordRegister with ProtectedModeIndexRegister

    case object LongSourceLowByte extends SourceIndex with LowByteRegister
    case object LongDestinationLowByte extends DestinationIndex with LowByteRegister
  }

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

    val SP: Pointer.RealSource.type = Pointer.RealSource
    val BP: Pointer.RealBase.type = Pointer.RealBase

    val SI: Index.RealSource.type = Index.RealSource
    val DI: Index.RealDestination.type = Index.RealDestination
  }

  trait I386Registers {
    val EAX: Accumulator.DoubleWord.type = Accumulator.DoubleWord
    val ECX: Count.DoubleWord.type = Count.DoubleWord
    val EDX: Data.DoubleWord.type = Data.DoubleWord
    val EBX: Base.DoubleWord.type = Base.DoubleWord

    val ESP: Pointer.ProtectedSource.type = Pointer.ProtectedSource
    val EBP: Pointer.ProtectedBase.type = Pointer.ProtectedBase

    val ESI: Index.ProtectedSource.type = Index.ProtectedSource
    val EDI: Index.ProtectedDestination.type = Index.ProtectedDestination
  }

  trait X64Registers {
    val RAX: Accumulator.QuadWord.type = Accumulator.QuadWord
    val RCX: Count.QuadWord.type = Count.QuadWord
    val RDX: Data.QuadWord.type = Data.QuadWord
    val RBX: Base.QuadWord.type = Base.QuadWord

    val RSP: Pointer.LongSource.type = Pointer.LongSource
    val RBP: Pointer.LongBase.type = Pointer.LongBase

    val SPL: Pointer.LongSourceLowByte.type = Pointer.LongSourceLowByte
    val BPL: Pointer.LongBaseLowByte.type = Pointer.LongBaseLowByte

    val RSI: Index.LongSource.type = Index.LongSource
    val RDI: Index.LongDestination.type = Index.LongDestination

    val SIL: Index.LongSourceLowByte.type = Index.LongSourceLowByte
    val DIL: Index.LongDestinationLowByte.type = Index.LongDestinationLowByte
  }

  object BaseIndexReference {
    object BX_SI extends BaseIndexReference(Register.Base.Word, Register.Index.RealSource, 0x00)
    object BX_DI extends BaseIndexReference(Register.Base.Word, Register.Index.RealDestination, 0x01)
    object BP_SI extends BaseIndexReference(Register.Pointer.RealBase, Register.Index.RealSource, 0x02)
    object BP_DI extends BaseIndexReference(Register.Pointer.RealBase, Register.Index.RealDestination, 0x03)
  }



  // I used Intel documentation for creating this, so I used the Intel notation for clarity from that perspective.
  // This might be confusing as the AMD notation seems more accepted. This might change in the future.
  case object R8L extends Rex8 with LowByteRegister
  case object R9L extends Rex9 with LowByteRegister
  case object R10L extends Rex10 with LowByteRegister
  case object R11L extends Rex11 with LowByteRegister
  case object R12L extends Rex12 with LowByteRegister
  case object R13L extends Rex13 with LowByteRegister
  case object R14L extends Rex14 with LowByteRegister
  case object R15L extends Rex15 with LowByteRegister

  case object R8W extends Rex8 with WordRegister
  case object R9W extends Rex9 with WordRegister
  case object R10W extends Rex10 with WordRegister
  case object R11W extends Rex11 with WordRegister
  case object R12W extends Rex12 with WordRegister
  case object R13W extends Rex13 with WordRegister
  case object R14W extends Rex14 with WordRegister
  case object R15W extends Rex15 with WordRegister

  case object R8D extends Rex8 with DoubleWordRegister with ProtectedModeIndexRegister
  case object R9D extends Rex9 with DoubleWordRegister with ProtectedModeIndexRegister
  case object R10D extends Rex10 with DoubleWordRegister with ProtectedModeIndexRegister
  case object R11D extends Rex11 with DoubleWordRegister with ProtectedModeIndexRegister
  case object R12D extends Rex12 with DoubleWordRegister
  case object R13D extends Rex13 with DoubleWordRegister with ProtectedModeIndexRegister
  case object R14D extends Rex14 with DoubleWordRegister with ProtectedModeIndexRegister
  case object R15D extends Rex15 with DoubleWordRegister with ProtectedModeIndexRegister

  case object R8 extends Rex8 with QuadWordRegister with ProtectedModeIndexRegister
  case object R9 extends Rex9 with QuadWordRegister with ProtectedModeIndexRegister
  case object R10 extends Rex10 with QuadWordRegister with ProtectedModeIndexRegister
  case object R11 extends Rex11 with QuadWordRegister with ProtectedModeIndexRegister
  case object R12 extends Rex12 with QuadWordRegister
  case object R13 extends Rex13 with QuadWordRegister with ProtectedModeIndexRegister
  case object R14 extends Rex14 with QuadWordRegister with ProtectedModeIndexRegister
  case object R15 extends Rex15 with QuadWordRegister with ProtectedModeIndexRegister

}