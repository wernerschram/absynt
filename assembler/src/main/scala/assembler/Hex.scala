package assembler

object Hex {
  val hexRadix = 16

  def lsb(string: String): List[Byte] =
    string.split(" ")
    .flatMap { x => x.grouped(2) }
  .toList.map { x => Integer.parseUnsignedInt(x, hexRadix).toByte }

  def msb(string: String): List[Byte] =
    string.split(" ")
    .flatMap { x => x.grouped(2).toList.reverse }
  .toList.map { x => Integer.parseUnsignedInt(x, hexRadix).toByte }
}
