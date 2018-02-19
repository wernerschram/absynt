package assembler.x86.operands

import assembler.x86.ProcessorMode

sealed abstract class Register extends Operand

sealed abstract class GeneralPurposeRegister(val registerCode: Byte, val mnemonic: String)
  extends Register with ModRMEncodableOperand with ValueSize2 {
  val modValue: Byte = 0x03.toByte
  val registerOrMemoryModeCode: Byte = registerCode
}

sealed abstract class GeneralPurposeRexRegister(registerCode: Byte, mnemonic: String)
  extends GeneralPurposeRegister(registerCode, mnemonic) {
  override def isValidForMode(processorMode: ProcessorMode): Boolean = processorMode == ProcessorMode.Long
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

sealed trait RegisterReference extends FixedSizeOperand {
  val defaultSegment: SegmentRegister = Register.DS
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

  val operandByteSize: OperandSize = index.operandByteSize

  override def toString = s"$base+$index"
}

object BaseIndexReference {
  object BX_SI extends BaseIndexReference(Register.BX, Register.SI, 0x00)
  object BX_DI extends BaseIndexReference(Register.BX, Register.DI, 0x01)
  object BP_SI extends BaseIndexReference(Register.BP, Register.SI, 0x02)
  object BP_DI extends BaseIndexReference(Register.BP, Register.DI, 0x03)
}

sealed trait BaseRegisterReference extends ModRMEncodableOperand with FixedSizeOperand {
  self: GeneralPurposeRegister =>

  def combinedIndex(index: CombinableRealModeIndexRegister): BaseIndexReference

  final def +(index: CombinableRealModeIndexRegister): BaseIndexReference =
    combinedIndex(index)
}

sealed trait ProtectedModeIndexRegister extends IndexRegister {
  self: GeneralPurposeRegister =>
  override val indexCode: Byte = self.registerOrMemoryModeCode
}

sealed trait SIBIndexRegister extends ModRMEncodableOperand with FixedSizeOperand {
  val defaultSIBSegment: SegmentRegister = Register.DS
  val SIBIndexCode: Byte = registerOrMemoryModeCode
}

sealed trait SIBBaseRegister extends ModRMEncodableOperand with FixedSizeOperand {
  val SIBBaseCode: Byte = registerOrMemoryModeCode
}

sealed trait ByteRegister extends GeneralPurposeRegister with ByteSize {
}

sealed trait LowByteRegister extends ByteRegister {
  override def toString: String = if ( mnemonic.startsWith("r")) s"${mnemonic}l" else mnemonic.replace('x', 'l')
}

sealed trait HighByteRegister extends ByteRegister {
  override val registerOrMemoryModeCode: Byte = (registerCode + 0x04).toByte

  override def toString: String = mnemonic.replace('x', 'h')
}

sealed trait WideRegister extends GeneralPurposeRegister with WideSize

sealed trait WordRegister extends WideRegister with WordSize {
  override def toString: String = if (mnemonic.startsWith("r")) s"${mnemonic}w" else mnemonic
}

sealed trait DoubleWordRegister extends WideRegister with SIBIndexRegister with SIBBaseRegister with DoubleWordSize {
  override def toString: String = if (mnemonic.startsWith("r")) s"${mnemonic}d" else s"e$mnemonic"
}

sealed trait QuadWordRegister extends WideRegister with SIBIndexRegister with SIBBaseRegister with QuadWordSize {
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
  case object BX extends BaseRegister with WordRegister with BaseRegisterReference with RealModeIndexRegister {
    override val indexCode: Byte = 0x07.toByte

    override def combinedIndex(index: CombinableRealModeIndexRegister): BaseIndexReference =
      index match {
        case SI => BaseIndexReference.BX_SI
        case DI => BaseIndexReference.BX_DI
      }

  }

  case object SP extends SourcePointer with WordRegister
  case object BP extends BasePointer with WordRegister with BaseRegisterReference with RealModeIndexRegister {
    override val indexCode: Byte = 0x06.toByte

    override val onlyWithDisplacement: Boolean = true

    override def combinedIndex(index: CombinableRealModeIndexRegister): BaseIndexReference =
      index match {
        case SI => BaseIndexReference.BP_SI
        case DI => BaseIndexReference.BP_DI
      }

  }

  final case object SI extends SourceIndex with WordRegister with CombinableRealModeIndexRegister {
    override val indexCode: Byte = 0x04.toByte
  }

  final case object DI extends DestinationIndex with WordRegister with CombinableRealModeIndexRegister {
    override val defaultSegment: SegmentRegister = Register.ES
    override val indexCode: Byte = 0x05.toByte
  }

  // i386 registers
  case object EAX extends AccumulatorRegister with DoubleWordRegister with ProtectedModeIndexRegister
  case object ECX extends CountRegister with DoubleWordRegister with ProtectedModeIndexRegister
  case object EDX extends DataRegister with DoubleWordRegister with ProtectedModeIndexRegister
  case object EBX extends BaseRegister with DoubleWordRegister with ProtectedModeIndexRegister
  case object ESP extends SourcePointer with DoubleWordRegister
  case object EBP extends BasePointer with DoubleWordRegister with ProtectedModeIndexRegister {
    override val onlyWithDisplacement: Boolean = true
  }
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