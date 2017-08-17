package assembler

import java.nio.ByteBuffer

object ListExtensions {

  implicit class ByteEncoder(value: Byte) {
    def encodeLittleEndian : List[Byte] = value :: Nil
    def encodeBigEndian : List[Byte] = value :: Nil
  }

  implicit class ShortEncoder(value: Short) {
    def encodeLittleEndian : List[Byte] =
      (0 to 1).map { x =>
        ((value >> (8 * x)) & 0xff.toByte).toByte
      }.toList

    def encodeBigEndian : List[Byte] = (1 to 0 by -1).map { x =>
        ((value >> (8 * x)) & 0xff.toByte).toByte
      }.toList
  }

  implicit class IntEncoder(value: Int) {
    def encodeLittleEndian : List[Byte] = (0 to 3).map { x =>
        ((value >> (8 * x)) & 0xff.toByte).toByte
      }.toList

    def encodeBigEndian : List[Byte] = (3 to 0 by -1).map { x =>
        ((value >> (8 * x)) & 0xff.toByte).toByte
      }.toList
    }

  implicit class LongEncoder(value: Long) {
    def encodeLittleEndian : List[Byte] = (0 to 7).map { x =>
        ((value >> (8 * x)) & 0xff.toByte).toByte
      }.toList

    def encodeBigEndian : List[Byte] = (7 to 0 by -1).map { x =>
        ((value >> (8 * x)) & 0xff.toByte).toByte
      }.toList
  }

  implicit class ListToImmediate(value: Seq[Byte]) {
    def decimalString: String = decimal.toString

    def bigEndianHexString : String = value.reverseMap("%02X" format _).mkString
    def hexString : String = value.map("%02X" format _).mkString

    def decimal: Long =
      ByteBuffer.wrap(value.padTo(java.lang.Long.BYTES, 0.toByte).take(java.lang.Long.BYTES).reverse.toArray).getLong
  }
}
