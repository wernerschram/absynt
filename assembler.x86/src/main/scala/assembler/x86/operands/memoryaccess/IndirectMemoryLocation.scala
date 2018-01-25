package assembler.x86.operands.memoryaccess

import assembler.x86.operands.{OperandSize, SegmentRegister}
import assembler.ListExtensions._

abstract class IndirectMemoryLocation(val registerOrMemoryModeCode: Byte, displacement: Seq[Byte] = Seq.empty[Byte],
                                      addressSize: OperandSize, segment: SegmentRegister)
  extends MemoryLocation(displacement, segment, addressSize) {

  val modValue: Byte = {
    assume((0 :: 1 :: 2 :: 4 :: Nil).contains(displacement.size))
    displacement.length match {
      case 0 => 0x00
      case 1 => 0x01
      case 2 | 4 => 0x02
    }
  }
}

sealed abstract class Displacement {
  def encode: Seq[Byte]
}

object Displacement {
  case object None extends Displacement {
    override def encode: Seq[Byte] = Seq.empty
  }

  implicit def apply(displacement: Byte): Displacement = new Displacement {
    override def encode: Seq[Byte] = Seq(displacement)
  }

  implicit def apply(displacement: Short): Displacement = new Displacement {
    override def encode: Seq[Byte] = displacement.encodeLittleEndian
  }

  implicit def apply(displacement: Int): Displacement = new Displacement {
    override def encode: Seq[Byte] = displacement.encodeLittleEndian
  }
}
