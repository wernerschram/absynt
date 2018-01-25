package assembler.x86.operands.memoryaccess

import assembler.x86.operands.{ModRMEncodableOperand, OperandSize, SegmentRegister}

abstract class MemoryLocation(val displacement: Displacement, val segment: SegmentRegister, val addressSize: OperandSize)
  extends ModRMEncodableOperand {

  assume(List(0, 1, 2, 4, 8).contains(displacement.encode.length))
  lazy val segmentOverride: Option[SegmentRegister] = if (segment == defaultSegment) None else Some(segment)
  val defaultSegment: SegmentRegister

  def segmentPrefix: String = segmentOverride match {
    case Some(segmentRegister) => s"$segmentRegister:"
    case None => ""
  }
}

