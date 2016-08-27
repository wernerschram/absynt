package assembler.arm.operands

import assembler.ListExtensions._
import scala.language.implicitConversions

class RelativePointer private(val displacement: Int) extends Operand {
//  assume(displacement.length == 3)
  override val toString = s"${(displacement * 4).toString()}"
}

object RelativePointer {
  implicit def apply(displacement: Int) = new RelativePointer(displacement/4)
}