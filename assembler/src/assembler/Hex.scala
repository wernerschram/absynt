package assembler

object Hex {
//  def apply(string: String): List[Byte] = LSB(string)

  def LSB(string: String): List[Byte] = string.split(" ").flatMap { x => x.grouped(2) }.toList.map { x => Integer.parseUnsignedInt(x, 16).toByte }

  def MSB(string: String): List[Byte] = string.split(" ").flatMap { x => x.grouped(2).toList.reverse }.toList.map { x => Integer.parseUnsignedInt(x, 16).toByte }
}