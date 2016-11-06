package assembler.x86.opcodes

import scala.collection.immutable.Map

import assembler.x86.ProcessorMode
import assembler.x86.RexExtendedRequirement
import assembler.x86.operands.Register
import assembler.x86.operands.SegmentRegister

object Opcode {
  private val OperandSizeCode = 0x66.toByte
  private val AddressSizeCode = 0x67.toByte
  val RepeatPrefix = 0xF3.toByte

  private val RexCode = 0x40.toByte
  private val RexWBitValue: Byte = 8

  private val SegmentOverrideCS = 0x2E.toByte
  private val SegmentOverrideSS = 0x36.toByte
  private val SegmentOverrideDS = 0x3E.toByte
  private val SegmentOverrideES = 0x26.toByte
  private val SegmentOverrideFS = 0x64.toByte
  private val SegmentOverrideGS = 0x65.toByte

  private val SegmentOverrideMap: Map[SegmentRegister, Byte] = Map(
    (Register.CS, Opcode.SegmentOverrideCS),
    (Register.SS, Opcode.SegmentOverrideSS),
    (Register.DS, Opcode.SegmentOverrideDS),
    (Register.ES, Opcode.SegmentOverrideES),
    (Register.FS, Opcode.SegmentOverrideFS),
    (Register.GS, Opcode.SegmentOverrideGS))

  def optionalOperandSizePrefix(operandSize: Option[Int])(implicit processorMode: ProcessorMode): List[Byte] =
    (operandSize, processorMode) match {
      case (Some(4), ProcessorMode.Real) => Opcode.OperandSizeCode :: Nil
      case (Some(2), ProcessorMode.Protected | ProcessorMode.Long) => Opcode.OperandSizeCode :: Nil
      case _ => Nil
    }

  def optionalAddressSizePrefix(addressSize: Option[Int])(implicit processorMode: ProcessorMode): List[Byte] = {
    (addressSize, processorMode) match {
      case (Some(2), ProcessorMode.Protected) => Opcode.AddressSizeCode :: Nil
      case (Some(4), ProcessorMode.Real) => Opcode.AddressSizeCode :: Nil
      case (Some(4), ProcessorMode.Long) => Opcode.AddressSizeCode :: Nil
      case _ => Nil
    }
  }

  def optionalSegmentOverridePrefix(segmentOverride: Option[SegmentRegister]): List[Byte] = segmentOverride match {
    case Some(segment) => Opcode.SegmentOverrideMap.get(segment).toList
    case _ => Nil
  }

  def optionalRexPrefix(operandSize: Option[Int], rexRequirements: List[RexExtendedRequirement], includeRexW: Boolean)(implicit processorMode: ProcessorMode): List[Byte] = {
    val rexW = (includeRexW && operandSize.isDefined && operandSize.get == 8)
    if (rexRequirements.isEmpty && (!rexW)) {
      Nil
    } else {
      val rexPrefix = rexRequirements.foldLeft[Byte](Opcode.RexCode)((value, req) => (value | req.rexBitmask).toByte)

      if (rexW) {
        (rexPrefix | Opcode.RexWBitValue).toByte :: Nil
      } else {
        rexPrefix :: Nil
      }
    }
  }
}
