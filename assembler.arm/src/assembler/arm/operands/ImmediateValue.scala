package assembler.arm.operands

import assembler.ListExtensions._
import scala.language.implicitConversions
import java.nio.ByteBuffer

final class ImmediateValue(val value: Byte) extends Operand {
  
  val isPositive = (value & 0x80.toByte) == 0
  
  override def toString() = value.toString()
}

object ImmediateValue {
  implicit def byteToImmediate(value: Byte): ImmediateValue = new ImmediateValue(value)
}