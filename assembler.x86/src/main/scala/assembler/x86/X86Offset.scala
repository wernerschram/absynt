package assembler.x86

import assembler.{Address, Offset}
import assembler.ListExtensions._

trait X86Offset extends Offset {
  def encodeBytes: List[Byte]
}
