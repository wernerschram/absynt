package assembler.arm.operands

import scala.language.implicitConversions

sealed class RelativePointer(val displacement: Int) extends Operand {
  //displacement should be between 3221225472 and -3221225473
  assume(((displacement & 0xC0000000) == 0) || ((displacement & 0xC0000000) == 0xC0000000))

  def encode = (displacement >> 2) & 0xFFFFFF

  override def toString() = s"${displacement.toString()}"
}

class RelativeA32Pointer private(displacement: Int) extends RelativePointer(displacement) {
  //displacement should be divisible by 4
  assume((displacement & 0x00000003) == 0)
}

class RelativeThumbPointer private(displacement: Int) extends RelativePointer(displacement) {
  //displacement should be divisible by 4
  assume((displacement & 0x00000002) == 0)

  override def encode = super.encode | ((displacement & 1) << 24)
}

object RelativeA32Pointer {
  implicit def apply(displacement: Int) = new RelativeA32Pointer(displacement)
}

object RelativeThumbPointer {
  implicit def apply(displacement: Int) = new RelativeThumbPointer(displacement)
}
