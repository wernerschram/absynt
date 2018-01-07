package assembler.x86.operands

import assembler.x86.{ParameterPosition, ProcessorMode, RexRequirement}

sealed abstract class Register extends Operand

sealed abstract class GeneralPurposeRegister(val registerCode: Byte, val mnemonic: String)
  extends Register with ModRMEncodableOperand with FixedSizeOperand {
  val modValue: Byte = 0x03.toByte
  val registerOrMemoryModeCode: Byte = registerCode
}

sealed abstract class GeneralPurposeRexRegister(registerCode: Byte, mnemonic: String)
  extends GeneralPurposeRegister(registerCode, mnemonic) {
  override def getRexRequirements(position: ParameterPosition): Seq[RexRequirement] =
    position.rexRequirement.toList ++ super.getRexRequirements(position)

  override def isValidForMode(processorMode: ProcessorMode): Boolean = processorMode == ProcessorMode.Long
}

sealed abstract class AccumulatorRegister extends GeneralPurposeRegister(0x00, "ax")
sealed abstract class CountRegister extends GeneralPurposeRegister(0x01, "cx")
sealed abstract class DataRegister extends GeneralPurposeRegister(0x02, "dx")
sealed abstract class BaseRegister extends GeneralPurposeRegister(0x03, "bx")
sealed abstract class SourcePointer extends GeneralPurposeRegister(0x04, "sp")
sealed abstract class BasePointer extends GeneralPurposeRegister(0x05, "bp")
sealed abstract class SourceIndex extends GeneralPurposeRegister(0x06, "si") with IndexRegister
sealed abstract class DestinationIndex extends GeneralPurposeRegister(0x07, "di") with IndexRegister

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

sealed trait BaseIndexPair extends ModRMEncodableOperand with FixedSizeOperand {
  val defaultSegment: SegmentRegister = Register.DS
  val indexCode: Byte = registerOrMemoryModeCode
}

sealed trait IndexRegister extends Register with BaseIndexPair

sealed trait RealModeIndexRegister extends IndexRegister

sealed trait ProtectedModeIndexRegister extends IndexRegister

sealed trait EncodedBaseRegister extends ModRMEncodableOperand with FixedSizeOperand {
  self: Register =>

  def getBaseCode(index: RealModeIndexRegister): Byte

  def combinedIndex(index: RealModeIndexRegister): BaseIndexPair = new BaseIndexPair {
    override val defaultSegment: SegmentRegister = index.defaultSegment
    override val indexCode: Byte = getBaseCode(index)

    val operandByteSize: OperandSize = index.operandByteSize

    val modValue: Byte = index.modValue
    val registerOrMemoryModeCode: Byte = getBaseCode(index)

    override def toString = s"${EncodedBaseRegister.this}+$index"
  }
}

sealed trait SIBIndexRegister extends ModRMEncodableOperand with FixedSizeOperand {
  val defaultSIBSegment: SegmentRegister = Register.DS
  val SIBIndexCode: Byte = registerOrMemoryModeCode
}

sealed trait SIBBaseRegister extends ModRMEncodableOperand with FixedSizeOperand {
  val SIBBaseCode: Byte = registerOrMemoryModeCode
}

sealed trait ByteRegister extends GeneralPurposeRegister {
  override val operandByteSize: OperandSize = ValueSize.Byte
}

sealed trait LowByteRegister extends ByteRegister {
  override def toString: String = if ( mnemonic.startsWith("r")) s"${mnemonic}l" else mnemonic.replace('x', 'l')
}

sealed trait HighByteRegister extends ByteRegister {
  override val registerOrMemoryModeCode: Byte = (registerCode + 0x04).toByte

  override def toString: String = mnemonic.replace('x', 'h')
}

sealed trait WideRegister extends GeneralPurposeRegister

sealed trait WordRegister extends WideRegister {
  override val operandByteSize: OperandSize = ValueSize.Word

  override def toString: String = if (mnemonic.startsWith("r")) s"${mnemonic}w" else mnemonic
}

sealed trait DoubleWordRegister extends WideRegister with SIBIndexRegister with SIBBaseRegister {
  override val operandByteSize: OperandSize = ValueSize.DoubleWord

  override def toString: String = if (mnemonic.startsWith("r")) s"${mnemonic}d" else s"e$mnemonic"
}

sealed trait QuadWordRegister extends WideRegister with SIBIndexRegister with SIBBaseRegister {
  override val operandByteSize: OperandSize = ValueSize.QuadWord

  override def toString: String = if (mnemonic.startsWith("r")) mnemonic else s"r$mnemonic"
}

object Register {

  // Small registers
  case object AL extends AccumulatorRegister with LowByteRegister
  case object CL extends CountRegister with LowByteRegister
  case object DL extends DataRegister with LowByteRegister
  case object BL extends BaseRegister with LowByteRegister
  case object AH extends AccumulatorRegister with HighByteRegister
  case object CH extends CountRegister with HighByteRegister
  case object DH extends DataRegister with HighByteRegister
  case object BH extends BaseRegister with HighByteRegister

  // Segment registers
  case object ES extends SegmentRegister(0x00, "es")
  case object CS extends SegmentRegister(0x01, "cs")
  case object SS extends SegmentRegister(0x02, "ss")
  case object DS extends SegmentRegister(0x03, "ds")
  case object FS extends SegmentRegister(0x04, "fs")
  case object GS extends SegmentRegister(0x05, "gs")

  // Wide registers
  case object AX extends AccumulatorRegister with WordRegister
  case object CX extends CountRegister with WordRegister
  case object DX extends DataRegister with WordRegister
  case object BX extends BaseRegister with WordRegister with EncodedBaseRegister with RealModeIndexRegister {
    override val indexCode: Byte = 0x07.toByte

    def getBaseCode(index: RealModeIndexRegister): Byte = {
      index match {
        case SI => 0x00
        case DI => 0x01
        case _ => throw new AssertionError
      }
    }
  }

  case object SP extends SourcePointer with WordRegister
  case object BP extends BasePointer with WordRegister with EncodedBaseRegister with RealModeIndexRegister {
    override val indexCode: Byte = 0x06.toByte

    def getBaseCode(index: RealModeIndexRegister): Byte = {
      index match {
        case SI => 0x02
        case DI => 0x03
        case _ => throw new AssertionError
      }
    }
  }

  final case object SI extends SourceIndex with WordRegister with RealModeIndexRegister {
    override val indexCode: Byte = 0x04.toByte
  }

  final case object DI extends DestinationIndex with WordRegister with RealModeIndexRegister {
    override val defaultSegment: SegmentRegister = Register.ES
    override val indexCode: Byte = 0x05.toByte
  }

  // i386 registers
  case object EAX extends AccumulatorRegister with DoubleWordRegister with ProtectedModeIndexRegister
  case object ECX extends CountRegister with DoubleWordRegister with ProtectedModeIndexRegister
  case object EDX extends DataRegister with DoubleWordRegister with ProtectedModeIndexRegister
  case object EBX extends BaseRegister with DoubleWordRegister with ProtectedModeIndexRegister
  case object ESP extends SourcePointer with DoubleWordRegister
  case object EBP extends BasePointer with DoubleWordRegister with ProtectedModeIndexRegister
  case object ESI extends SourceIndex with DoubleWordRegister with ProtectedModeIndexRegister {
    override val defaultSegment: SegmentRegister = Register.ES
  }
  case object EDI extends DestinationIndex with DoubleWordRegister with ProtectedModeIndexRegister

  // x86-64 registers
  case object RAX extends AccumulatorRegister with QuadWordRegister with ProtectedModeIndexRegister
  case object RCX extends CountRegister with QuadWordRegister with ProtectedModeIndexRegister
  case object RDX extends DataRegister with QuadWordRegister with ProtectedModeIndexRegister
  case object RBX extends BaseRegister with QuadWordRegister with ProtectedModeIndexRegister
  case object RSP extends SourcePointer with QuadWordRegister
  case object RBP extends BasePointer with QuadWordRegister with ProtectedModeIndexRegister
  case object RSI extends SourceIndex with QuadWordRegister with ProtectedModeIndexRegister {
    override val defaultSegment: SegmentRegister = Register.ES
  }
  case object RDI extends DestinationIndex with QuadWordRegister with ProtectedModeIndexRegister

  case object SPL extends SourcePointer with LowByteRegister
  case object BPL extends BasePointer with LowByteRegister
  case object SIL extends SourceIndex with LowByteRegister
  case object DIL extends DestinationIndex with LowByteRegister

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