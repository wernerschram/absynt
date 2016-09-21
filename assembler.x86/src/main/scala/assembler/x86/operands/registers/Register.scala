package assembler.x86.operands.registers

import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.RexExtendedRequirement
import assembler.x86.operands.FixedSizeEncodableOperand
import assembler.x86.operands.FixedSizeModRMEncodableOperand
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.Operand

/**
 * @author werners
 */

sealed abstract class Register extends Operand

sealed abstract class EncodableRegister(registerCode: Byte) extends Register with FixedSizeModRMEncodableOperand {

  val modValue = 0x03.toByte
  val registerOrMemoryModeCode = registerCode
  val displacement = List.empty[Byte]
}

sealed abstract class GeneralRegister(registerCode: Byte, val operandByteSize: Int) extends EncodableRegister(registerCode) {
}

trait AccumulatorRegister extends GeneralRegister
trait CountRegister extends GeneralRegister
trait DataRegister extends GeneralRegister
trait BaseRegister extends GeneralRegister

trait SourcePointer extends GeneralRegister
trait BasePointer extends GeneralRegister
trait SourceIndex extends GeneralRegister with IndexRegister
sealed trait DestinationIndex extends GeneralRegister with IndexRegister

sealed abstract class ByteRegister(registerCode: Byte) extends GeneralRegister(registerCode, 1) {
  override def getRexRequirements(position: ParameterPosition): List[RexExtendedRequirement] = Nil
}

sealed abstract class HighByteRegister(registerCode: Byte) extends ByteRegister(registerCode)

sealed abstract class RexByteRegister(registerCode: Byte) extends ByteRegister(registerCode) {
  override def getRexRequirements(position: ParameterPosition): List[RexExtendedRequirement] =
    RexExtendedRequirement.instance(position) :: Nil
  override def isValidForMode(processorMode: ProcessorMode): Boolean = processorMode == ProcessorMode.Long
}

sealed abstract class WideRegister(registerCode: Byte, operandBytes: Byte) extends GeneralRegister(registerCode, operandBytes) {
}

sealed abstract class WordRegister(registerCode: Byte) extends WideRegister(registerCode, 2) {
  override def getRexRequirements(position: ParameterPosition): List[RexExtendedRequirement] =
    Nil
}

sealed abstract class RexWordRegister(registerCode: Byte) extends WordRegister(registerCode) {
  override def getRexRequirements(position: ParameterPosition): List[RexExtendedRequirement] =
    RexExtendedRequirement.instance(position) :: Nil
  override def isValidForMode(processorMode: ProcessorMode): Boolean = processorMode == ProcessorMode.Long
}

sealed abstract class DoubleWordRegister(registerCode: Byte)
    extends WideRegister(registerCode, 4) with SIBIndexRegister with SIBBaseRegister {
  override def getRexRequirements(position: ParameterPosition): List[RexExtendedRequirement] =
    Nil
}

sealed abstract class RexDoubleWordRegister(registerCode: Byte) extends DoubleWordRegister(registerCode) {
  override def getRexRequirements(position: ParameterPosition): List[RexExtendedRequirement] =
    RexExtendedRequirement.instance(position) :: Nil
  override def isValidForMode(processorMode: ProcessorMode): Boolean = processorMode == ProcessorMode.Long
}

sealed abstract class QuadWordRegister(registerCode: Byte)
    extends WideRegister(registerCode, 8) with SIBIndexRegister with SIBBaseRegister {
  override def getRexRequirements(position: ParameterPosition): List[RexExtendedRequirement] =
    Nil
  override def isValidForMode(processorMode: ProcessorMode): Boolean = processorMode == ProcessorMode.Long
}

sealed abstract class RexQuadWordRegister(registerCode: Byte) extends QuadWordRegister(registerCode) {
  override def getRexRequirements(position: ParameterPosition): List[RexExtendedRequirement] =
    RexExtendedRequirement.instance(position) :: Nil
  override def isValidForMode(processorMode: ProcessorMode): Boolean = processorMode == ProcessorMode.Long
}

//sealed abstract class MMXRegister(registerCode : Byte) extends Register(registerCode, 16)
//
//sealed abstract class XMMRegister(registerCode : Byte) extends Register(registerCode, 32)

sealed abstract class SegmentRegister(val registerCode: Byte) extends Register {
  val operandByteSize: Int = 2
  override def getRexRequirements(position: ParameterPosition): List[RexExtendedRequirement] =
    Nil
}

trait EncodedBaseRegister extends FixedSizeEncodableOperand {
  def getBaseCode(index: RealModeIndexRegister): Byte

  def combinedIndex(index: RealModeIndexRegister): IndexRegister = new IndexRegister {
    override val defaultSegment = index.defaultSegment
    override val indexCode = getBaseCode(index)

    val operandByteSize: Int = index.operandByteSize

    val modValue: Byte = index.modValue
    val registerOrMemoryModeCode: Byte = getBaseCode(index)

    def getRexRequirements(position: assembler.x86.ParameterPosition): List[assembler.x86.RexExtendedRequirement] =
      EncodedBaseRegister.this.getRexRequirements(ParameterPosition.Base) ::: index.getRexRequirements(ParameterPosition.Index)
  }
}

abstract trait IndexRegister extends FixedSizeEncodableOperand with ModRMEncodableOperand {
  val defaultSegment: SegmentRegister = Register.DS
  val indexCode: Byte = registerOrMemoryModeCode
  val displaceOnly: Boolean = false
}

trait RealModeIndexRegister extends IndexRegister

trait ProtectedModeIndexRegister extends IndexRegister

trait SIBIndexRegister extends FixedSizeEncodableOperand with ModRMEncodableOperand {
  val defaultSIBSegment: SegmentRegister = Register.DS
  val SIBIndexCode: Byte = registerOrMemoryModeCode
}

trait SIBBaseRegister extends FixedSizeEncodableOperand with ModRMEncodableOperand {
  val SIBBaseCode: Byte = registerOrMemoryModeCode
  val scaleOnly: Boolean = false

  protected def SIBBaseCode(scale: Byte): Byte = {
    assume(scaleOnly && scale == 0)
    SIBBaseCode
  }
}

object Register {
  // i8086 registers
  val nonRexMatcher: PartialFunction[Register, Boolean] = { case AL | CL | DL | BL | AH | CH | DH | BH => true; case _ => false }

  case object AL extends ByteRegister(0x00) with AccumulatorRegister
  case object CL extends ByteRegister(0x01) with CountRegister
  case object DL extends ByteRegister(0x02) with DataRegister
  case object BL extends ByteRegister(0x03) with BaseRegister
  case object AH extends HighByteRegister(0x04) with AccumulatorRegister
  case object CH extends HighByteRegister(0x05) with CountRegister
  case object DH extends HighByteRegister(0x06) with DataRegister
  case object BH extends HighByteRegister(0x07) with BaseRegister

  case object AX extends WordRegister(0x00) with AccumulatorRegister
  case object CX extends WordRegister(0x01) with CountRegister
  case object DX extends WordRegister(0x02) with DataRegister
  case object BX extends WordRegister(0x03) with EncodedBaseRegister with RealModeIndexRegister with BaseRegister {
    override val indexCode = 0x07.toByte
    def getBaseCode(index: RealModeIndexRegister) = {
      assume(index == SI || index == DI)
      index match {
        case SI => 0x00
        case DI => 0x01
      }
    }
  }
  case object SP extends WordRegister(0x04) with SourcePointer
  case object BP extends WordRegister(0x05) with EncodedBaseRegister with RealModeIndexRegister with BasePointer {
    override val indexCode = 0x06.toByte
    def getBaseCode(index: RealModeIndexRegister) = {
      assume(index == SI || index == DI)
      index match {
        case SI => 0x02
        case DI => 0x03
      }
    }
  }
  final case object SI extends WordRegister(0x06) with RealModeIndexRegister with SourceIndex {
    override val indexCode = 0x04.toByte
  }
  final case object DI extends WordRegister(0x07) with RealModeIndexRegister with DestinationIndex {
    override val defaultSegment = Register.ES
    override val indexCode = 0x05.toByte
  }

  // i386 registers
  case object EAX extends DoubleWordRegister(0x00) with ProtectedModeIndexRegister with AccumulatorRegister
  case object ECX extends DoubleWordRegister(0x01) with ProtectedModeIndexRegister with CountRegister
  case object EDX extends DoubleWordRegister(0x02) with ProtectedModeIndexRegister with DataRegister
  case object EBX extends DoubleWordRegister(0x03) with ProtectedModeIndexRegister with BaseRegister
  case object ESP extends DoubleWordRegister(0x04) with SourcePointer
  case object EBP extends DoubleWordRegister(0x05) with ProtectedModeIndexRegister with BasePointer {
    override val displaceOnly = true
    override val scaleOnly = true
  }
  case object ESI extends DoubleWordRegister(0x06) with ProtectedModeIndexRegister with SourceIndex {
    override val defaultSegment = Register.ES
  }
  case object EDI extends DoubleWordRegister(0x07) with ProtectedModeIndexRegister with DestinationIndex

  // MMX and SSE2 is for later...
  //
  //  case object MM0  extends MMXRegister(0x00)
  //  case object MM1  extends MMXRegister(0x01)
  //  case object MM2  extends MMXRegister(0x02)
  //  case object MM3  extends MMXRegister(0x03)
  //  case object MM4  extends MMXRegister(0x04)
  //  case object MM5  extends MMXRegister(0x05)
  //  case object MM6  extends MMXRegister(0x06)
  //  case object MM7  extends MMXRegister(0x07)
  //
  //  case object XMM0 extends XMMRegister(0x00)
  //  case object XMM1 extends XMMRegister(0x01)
  //  case object XMM2 extends XMMRegister(0x02)
  //  case object XMM3 extends XMMRegister(0x03)
  //  case object XMM4 extends XMMRegister(0x04)
  //  case object XMM5 extends XMMRegister(0x05)
  //  case object XMM6 extends XMMRegister(0x06)
  //  case object XMM7 extends XMMRegister(0x07)
  //
  //  case object XMM8  extends XMMRegister(0x00, true)
  //  case object XMM9  extends XMMRegister(0x01, true)
  //  case object XMM10 extends XMMRegister(0x02, true)
  //  case object XMM11 extends XMMRegister(0x03, true)
  //  case object XMM12 extends XMMRegister(0x04, true)
  //  case object XMM13 extends XMMRegister(0x05, true)
  //  case object XMM14 extends XMMRegister(0x06, true)
  //  case object XMM15 extends XMMRegister(0x07, true)

  case object ES extends SegmentRegister(0x00)
  case object CS extends SegmentRegister(0x01)
  case object SS extends SegmentRegister(0x02)
  case object DS extends SegmentRegister(0x03)
  case object FS extends SegmentRegister(0x04)
  case object GS extends SegmentRegister(0x05)

  // x86-64 registers
  case object RAX extends QuadWordRegister(0x00) with ProtectedModeIndexRegister with AccumulatorRegister
  case object RCX extends QuadWordRegister(0x01) with ProtectedModeIndexRegister with CountRegister
  case object RDX extends QuadWordRegister(0x02) with ProtectedModeIndexRegister with DataRegister
  case object RBX extends QuadWordRegister(0x03) with ProtectedModeIndexRegister with BaseRegister
  case object RSP extends QuadWordRegister(0x04) with SourcePointer
  case object RBP extends QuadWordRegister(0x05) with ProtectedModeIndexRegister with BasePointer {
    override val scaleOnly = true
  }
  case object RSI extends QuadWordRegister(0x06) with ProtectedModeIndexRegister with SourceIndex {
    override val defaultSegment = Register.ES
  }
  case object RDI extends QuadWordRegister(0x07) with ProtectedModeIndexRegister with DestinationIndex

  case object SPL extends ByteRegister(0x04)
  case object BPL extends ByteRegister(0x05)
  case object SIL extends ByteRegister(0x06)
  case object DIL extends ByteRegister(0x07)

  // I used Intel documentation for creating this, so I used the Intel notation for clarity from that perspective.
  // This might be confusing as the AMD notation seems more accepted. This might change in the future.
  case object R8L extends RexByteRegister(0x00)
  case object R9L extends RexByteRegister(0x01)
  case object R10L extends RexByteRegister(0x02)
  case object R11L extends RexByteRegister(0x03)
  case object R12L extends RexByteRegister(0x04)
  case object R13L extends RexByteRegister(0x05)
  case object R14L extends RexByteRegister(0x06)
  case object R15L extends RexByteRegister(0x07)

  case object R8W extends RexWordRegister(0x00)
  case object R9W extends RexWordRegister(0x01)
  case object R10W extends RexWordRegister(0x02)
  case object R11W extends RexWordRegister(0x03)
  case object R12W extends RexWordRegister(0x04)
  case object R13W extends RexWordRegister(0x05)
  case object R14W extends RexWordRegister(0x06)
  case object R15W extends RexWordRegister(0x07)

  case object R8D extends RexDoubleWordRegister(0x00) with ProtectedModeIndexRegister
  case object R9D extends RexDoubleWordRegister(0x01) with ProtectedModeIndexRegister
  case object R10D extends RexDoubleWordRegister(0x02) with ProtectedModeIndexRegister
  case object R11D extends RexDoubleWordRegister(0x03) with ProtectedModeIndexRegister
  case object R12D extends RexDoubleWordRegister(0x04)
  case object R13D extends RexDoubleWordRegister(0x05) with ProtectedModeIndexRegister {
    override val displaceOnly = true
    override val scaleOnly = true
  }
  case object R14D extends RexDoubleWordRegister(0x06) with ProtectedModeIndexRegister
  case object R15D extends RexDoubleWordRegister(0x07) with ProtectedModeIndexRegister

  case object R8 extends RexQuadWordRegister(0x00) with ProtectedModeIndexRegister
  case object R9 extends RexQuadWordRegister(0x01) with ProtectedModeIndexRegister
  case object R10 extends RexQuadWordRegister(0x02) with ProtectedModeIndexRegister
  case object R11 extends RexQuadWordRegister(0x03) with ProtectedModeIndexRegister
  case object R12 extends RexQuadWordRegister(0x04)
  case object R13 extends RexQuadWordRegister(0x05) with ProtectedModeIndexRegister {
    override val scaleOnly = true
  }
  case object R14 extends RexQuadWordRegister(0x06) with ProtectedModeIndexRegister
  case object R15 extends RexQuadWordRegister(0x07) with ProtectedModeIndexRegister
}