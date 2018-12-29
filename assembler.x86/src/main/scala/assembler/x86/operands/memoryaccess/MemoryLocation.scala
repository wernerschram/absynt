package assembler.x86.operands.memoryaccess

import assembler.x86.operands._
import assembler.x86.operations.AddressOperandInfo

abstract class MemoryLocation(val displacement: Option[ImmediateValue], val segment: SegmentRegister)
  extends ModRMEncodableOperand {

  def addressOperands: Set[AddressOperandInfo]

  def defaultSegment: SegmentRegister
  lazy val segmentOverride: Option[SegmentRegister] = if (segment == defaultSegment) None else Some(segment)

  def segmentPrefix: String = segmentOverride match {
    case Some(segmentRegister) => s"$segmentRegister:"
    case None => ""
  }
}

