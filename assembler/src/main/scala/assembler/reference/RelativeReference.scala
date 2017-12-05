package assembler.reference

import assembler._

trait RelativeReference
    extends Reference {

  final def encodeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable =
    offsetDirection match {
      case direction: RelativeOffsetDirection => encodeForDistance(dependencySize, direction)
      case _ => throw new AssertionError()
    }

  def encodeForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Encodable

  def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int

}
