package assembler.x86.operands.memoryaccess

import assembler.x86.operands.{OperandSize, SegmentRegister}
import assembler.ListExtensions._

abstract class IndirectMemoryLocation(val registerOrMemoryModeCode: Byte, displacement: Option[Displacement] = None,
                                      addressSize: OperandSize, segment: SegmentRegister)
  extends MemoryLocation(displacement, segment, addressSize) {

  val modValue: Byte = {
    displacement match {
      case None => 0x00
      case Some(d) if d.encode.lengthCompare(1) == 0 => 0x01
      case Some(d) if d.encode.lengthCompare(2) == 0 => 0x02
      case Some(d) if d.encode.lengthCompare(4) == 0 => 0x02
      case _ => throw new AssertionError
    }
  }
}

sealed abstract class Displacement {
  def encode: Seq[Byte]
}

object Displacement {

  implicit def apply(displacement: Byte): Displacement = new Displacement {
    override def encode: Seq[Byte] = Seq(displacement)
  }

  implicit def apply(displacement: Short): Displacement = new Displacement {
    override def encode: Seq[Byte] = displacement.encodeLittleEndian
  }

  implicit def apply(displacement: Int): Displacement = new Displacement {
    override def encode: Seq[Byte] = displacement.encodeLittleEndian
  }

  implicit def apply(displacement: Long): Displacement = new Displacement {
    override def encode: Seq[Byte] = displacement.encodeLittleEndian
  }
}
