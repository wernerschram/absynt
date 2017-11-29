package assembler.reference

import assembler._

trait SinglePassRelativeReference
    extends Reference {

  final def encodeForDistance(distance: Int, offsetDirection: OffsetDirection): Encodable =
    offsetDirection match {
      case direction: RelativeOffsetDirection => encodeForDistance(distance, direction)
      case _ => throw new AssertionError()
    }

  def encodeForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Encodable

  def sizeForDistance(distance: Int, offsetDirection: OffsetDirection): Int

}
