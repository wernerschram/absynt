package assembler

object Hex {
  val hexRadix = 16

  def lsb(string: String): Seq[Byte] =
    string.split(" ")
    .flatMap { x => x.grouped(2) }
  .toList.map { x =>
      assert(x.length == 2)
      Integer.parseUnsignedInt(x, hexRadix).toByte
    }

  def msb(string: String): Seq[Byte] =
    string.split(" ")
    .flatMap { x => x.grouped(2).toList.reverse }
  .toList.map { x =>
      assert(x.length == 2)
      Integer.parseUnsignedInt(x, hexRadix).toByte
    }
}
