package assembler

import java.nio.ByteBuffer

object ListExtensions {
  
  implicit class ByteEncoder(value: Byte) { 
    def encodeLittleEndian : List[Byte] = value :: Nil
    def encodeBigEndian : List[Byte] = value :: Nil
  }
    
  implicit class ShortEncoder(value: Short) { 
    def encodeLittleEndian : List[Byte] = {
      return (0 to 1).map { x =>
        ((value >> (8 * x)) & 0xff.toByte).toByte
      }.toList
    }
    
    def encodeBigEndian : List[Byte] = {
      return (1 to 0 by -1).map { x =>
        ((value >> (8 * x)) & 0xff.toByte).toByte
      }.toList
    }
  }
  
  implicit class IntEncoder(value: Int) { 
    def encodeLittleEndian : List[Byte] = {
      return (0 to 3).map { x =>
        ((value >> (8 * x)) & 0xff.toByte).toByte
      }.toList
    }
    
    def encodeBigEndian : List[Byte] = {
      return (3 to 0 by -1).map { x =>
        ((value >> (8 * x)) & 0xff.toByte).toByte
      }.toList
    }
  }

  implicit class LongEncoder(value: Long) { 
    def encodeLittleEndian : List[Byte] = {
      return (0 to 7).map { x =>
        ((value >> (8 * x)) & 0xff.toByte).toByte
      }.toList
    }
    
    def encodeBigEndian : List[Byte] = {
      return (7 to 0 by -1).map { x =>
        ((value >> (8 * x)) & 0xff.toByte).toByte
      }.toList
    }
  }
  
  implicit class ListToImmediate(value: List[Byte]) {
    def decimalString() = decimal.toString()

    def hexString() : String = value.reverse.map("%02X" format _).mkString
    
    def decimal(): Long = value.length match {
      case 1 => value.head
      case 2 => ByteBuffer.wrap(value.reverse.toArray).getShort
      case 3 => ByteBuffer.wrap((value ::: 0.toByte :: Nil).reverse.toArray).getInt
      case 4 => ByteBuffer.wrap(value.reverse.toArray).getInt
      case 8 => ByteBuffer.wrap(value.reverse.toArray).getLong
      case default => ByteBuffer.wrap(value.take(8).reverse.toArray).getLong
    }
  }
}