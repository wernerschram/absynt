package assembler.reference

import assembler._

trait RelativeReference
    extends Reference {

  final def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable =
    offsetDirection match {
      case direction: RelativeOffsetDirection => encodableForDistance(dependencySize, direction)
      case _ => throw new AssertionError()
    }

  def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Encodable

  def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int

}
