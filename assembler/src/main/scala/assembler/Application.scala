package assembler

import assembler.resource._
import assembler.sections.Section

abstract class Application {

  def sections: Seq[Section]

  def alignmentFillers: Map[Section, AlignmentFiller]

  def startOffset: Int

  val startFiller: Encodable = new Encodable {
    override def encodeByte: Seq[Byte] = Seq.empty

    override def size: Int = startOffset

    override def toString: String = "start offset filler"
  }

  def sectionDependencies(section: Section): Seq[Resource] =
    sections.takeWhile(_ != section).flatMap(s => alignmentFillers(s) +: s.content)

  def alignedSectionDependencies(section: Section): Seq[Resource] =
    sectionDependencies(section) :+ alignmentFillers(section)

  def encodeByte: Seq[Byte]

  def encodablesForDependencies(references: Seq[TempDependentResource]): Map[TempDependentResource, Encodable] = {
    val (totalDependencySizes: Map[TempDependentResource, DependencySize], restrictions: Map[TempDependentResource, Set[Int]]) =
      references.foldLeft((Map.empty[TempDependentResource, DependencySize], Map.empty[TempDependentResource, Set[Int]])) {
        case ((
            currentDependencySizeFunctions: Map[TempDependentResource, DependencySize],
            currentRestrictions: Map[TempDependentResource, Set[Int]]),
            currentReference: TempDependentResource) =>
          val (newDependencySizeFunctions, newRestrictions) =
            dependencySizes(Set.empty, currentDependencySizeFunctions, currentRestrictions)(currentReference)

          (currentDependencySizeFunctions ++ newDependencySizeFunctions, currentRestrictions ++ newRestrictions)
      }

    val validCombinations = possibleSizeCombinations(restrictions).filter(c => c == c.map{
        case (reference, _) =>
          reference -> reference.sizeForDependencySize(totalDependencySizes(reference).size(c), totalDependencySizes(reference).offsetDirection)
      })

    // TODO Handle the possibility that no combination is valid
    assert(validCombinations.nonEmpty)

    val shortestCombination = validCombinations.minBy(_.values.sum)

    references.map(reference =>
      reference -> reference.encodableForDependencySize(
        totalDependencySizes(reference).size(shortestCombination),
        totalDependencySizes(reference).offsetDirection))
      .toMap
  }

  private def possibleSizeCombinations(references: Map[TempDependentResource, Set[Int]]): Set[Map[TempDependentResource, Int]]  =
    if (references.isEmpty)
      Set(Map.empty)
    else
      for (
        t <- possibleSizeCombinations(references.tail);
        h <- references.head._2
      ) yield
        t + (references.head._1 -> h)

  private sealed abstract class DependencySize(val offsetDirection: OffsetDirection) {
    def size(assumptions: Map[TempDependentResource, Int]): Int
  }

  private case class KnownDependencySize(size: Int, override val offsetDirection: OffsetDirection) extends DependencySize(offsetDirection) {
    override def size(assumptions: Map[TempDependentResource, Int]): Int = size
  }

  private case class UnknownDependencySize(
    sizeFunction: Map[TempDependentResource, Int] => Int,
    override val offsetDirection: OffsetDirection
  ) extends DependencySize(offsetDirection) {
    override def size(assumptions: Map[TempDependentResource, Int]): Int = sizeFunction(assumptions)
  }

  private final def dependencySizes(visiting: Set[TempDependentResource],
    visited: Map[TempDependentResource, DependencySize], restrictions: Map[TempDependentResource, Set[Int]])(current: TempDependentResource):
      (Map[TempDependentResource, DependencySize], Map[TempDependentResource, Set[Int]]) = {

    if (visited.contains(current))
      // this reference has been evaluated in an earlier call (in a prior branch)
      (Map.empty, Map.empty)
    else {
      val (references, independentSizes, offsetDirection) = current.applicationContextProperties(this)

      val (totalDependencySizes, fixedSize, childSizeFunctions, totalRestrictions) =
        references.foldLeft((visited, independentSizes, Seq.empty[Map[TempDependentResource, Int] => Int], Map.empty[TempDependentResource, Set[Int]])) {
          case ((previousDependencySizeFunctions, previousFixedSize, previousChildSizeFunctions, previousRestrictions), child: TempDependentResource) =>
            childDependencies(visiting, current)(previousDependencySizeFunctions, previousFixedSize, previousChildSizeFunctions, previousRestrictions)(child)
        }

      if (childSizeFunctions.isEmpty)
        (totalDependencySizes + (current -> KnownDependencySize(fixedSize, offsetDirection)), restrictions ++ totalRestrictions)
      else {
        val dependencySize = (assumptions: Map[TempDependentResource, Int]) => fixedSize + childSizeFunctions.map(_(assumptions)).sum
        // TODO: Deactivated optimization. investigation needed.
//        if (totalRestrictions.contains(current)) {
//          val combinations = possibleSizeCombinations(totalRestrictions)
//          val sizes: Set[Int] = combinations.map(c => current.sizeForDependencySize(dependencySize(c), offsetDirection))
//          (totalDependencySizes + (current -> UnknownDependencySize(dependencySize, offsetDirection)), restrictions ++ totalRestrictions.updated(current, sizes))
//        } else
          (totalDependencySizes + (current -> UnknownDependencySize(dependencySize, offsetDirection)), restrictions ++ totalRestrictions)
      }
    }
  }

  @inline // inline to reduce stack size of the recursive dependencySizes call
  private final def childDependencies
    (visiting: Set[TempDependentResource],
      current: TempDependentResource)
    (previousDependencySizeFunctions: Map[TempDependentResource, DependencySize],
      previousFixedSize: Int,
      previousChildSizeFunctions: Seq[Map[TempDependentResource, Int] => Int],
      previousRestrictions: Map[TempDependentResource, Set[Int]])
    (child: TempDependentResource) = {

    if (visiting.contains(child))
    // cyclic dependency: add a dependency which can be resolved at a higher level
      (previousDependencySizeFunctions, previousFixedSize, previousChildSizeFunctions :+ ((assumptions: Map[TempDependentResource, Int]) =>
        assumptions(child)), previousRestrictions + (child -> child.possibleSizes))
    else {
      val (childSizeFunctions, childRestrictions) = dependencySizes(visiting + current, previousDependencySizeFunctions, previousRestrictions)(child)
      val newDependencySizes = previousDependencySizeFunctions ++ childSizeFunctions
      val newRestrictions = previousRestrictions ++ childRestrictions
      val dependencySize = newDependencySizes(child)
      dependencySize match {
        case known: KnownDependencySize =>
          val size = child.sizeForDependencySize(known.size, known.offsetDirection)
          (newDependencySizes, previousFixedSize + size, previousChildSizeFunctions, newRestrictions)
        case unknown: UnknownDependencySize =>
          val size = (assumptions: Map[TempDependentResource, Int]) => assumptions.getOrElse(child,
            child.sizeForDependencySize(unknown.sizeFunction(assumptions), unknown.offsetDirection))
          (newDependencySizes, previousFixedSize, previousChildSizeFunctions :+ size, newRestrictions)
      }
    }
  }
}
