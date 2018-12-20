package assembler.resource

import assembler.sections.Section
import assembler.{Application, EncodedBytes, OffsetDirection}

final case class AlignmentFiller(section: Section) extends UnlabeledDependentResource {

  def dependencies(context: Application): (Seq[Resource], OffsetDirection) =
    (context.startFiller +: context.sectionDependencies(section), OffsetDirection.Absolute)

  override def unlabeledForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): UnlabeledEncodable =
    EncodedBytes(Seq.fill(sizeForDependencySize(dependencySize, offsetDirection))(0.toByte))

  override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int = {
    val alignment = dependencySize % section.alignment
    if (alignment != 0)
      section.alignment - alignment
    else 0
  }

  override def possibleSizes: Set[Int] = (0 to section.alignment by 1).toSet

  override def toString: String = s"filler for ${section.name}"
}

