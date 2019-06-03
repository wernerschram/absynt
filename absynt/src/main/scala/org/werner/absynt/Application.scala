package org.werner.absynt

import org.werner.absynt.resource._
import org.werner.absynt.sections.Section
import org.werner.absynt.resource.{AlignmentFiller, DependentResource, Encodable, Resource, UnlabeledEncodable}
import org.werner.absynt.sections.Section

abstract class Application {

  def sections: Seq[Section]

  def alignmentFillers: Map[Section, AlignmentFiller]

  def startOffset: Int

  // Virtual filler with a set size for defining where the start of the application is including the size of
  // the application headers without including the actual application headers
  val startFiller: UnlabeledEncodable = new UnlabeledEncodable {
    override def encodeByte: Seq[Byte] = Seq.empty

    override def size: Int = startOffset

    override def toString: String = "start offset filler"
  }

  def sectionDependencies(section: Section): Seq[Resource] =
    sections.takeWhile(_ != section).flatMap(s => alignmentFillers(s) +: s.content)

  def alignedSectionDependencies(section: Section): Seq[Resource] =
    sectionDependencies(section) :+ alignmentFillers(section)

  def encodablesForDependencies(references: Seq[DependentResource]): Map[DependentResource, Encodable] = {
    val (totalDependencySizes: Map[DependentResource, DependencySize], restrictions: Map[DependentResource, Set[Int]]) =
      references.foldLeft((Map.empty[DependentResource, DependencySize], Map.empty[DependentResource, Set[Int]])) {
        case ((
            currentDependencySizeFunctions: Map[DependentResource, DependencySize],
            currentRestrictions: Map[DependentResource, Set[Int]]),
            currentReference: DependentResource) =>
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

  private def possibleSizeCombinations(references: Map[DependentResource, Set[Int]]): Set[Map[DependentResource, Int]]  =
    if (references.isEmpty)
      Set(Map.empty)
    else
      for (
        t <- possibleSizeCombinations(references.tail);
        (resource, sizes) = references.head;
        size <- sizes
      ) yield
        t + (resource -> size)

  private sealed abstract class DependencySize(val offsetDirection: OffsetDirection) {
    def size(assumptions: Map[DependentResource, Int]): Int
  }

  private case class KnownDependencySize(size: Int, override val offsetDirection: OffsetDirection) extends DependencySize(offsetDirection) {
    override def size(assumptions: Map[DependentResource, Int]): Int = size
  }

  private case class UnknownDependencySize(
    sizeFunction: Map[DependentResource, Int] => Int,
    override val offsetDirection: OffsetDirection
  ) extends DependencySize(offsetDirection) {
    override def size(assumptions: Map[DependentResource, Int]): Int = sizeFunction(assumptions)
  }

  private final def dependencySizes(visiting: Set[DependentResource],
    visited: Map[DependentResource, DependencySize], restrictions: Map[DependentResource, Set[Int]])(current: DependentResource):
      (Map[DependentResource, DependencySize], Map[DependentResource, Set[Int]]) = {

    if (visited.contains(current))
      // this reference has been evaluated in an earlier call (in a prior branch)
      (Map.empty, Map.empty)
    else {
      val (references, independentSizes, offsetDirection) = current.applicationContextProperties(this)

      val (totalDependencySizes, fixedSize, childSizeFunctions, totalRestrictions) =
        references.foldLeft((visited, independentSizes, Seq.empty[Map[DependentResource, Int] => Int], Map.empty[DependentResource, Set[Int]])) {
          case ((previousDependencySizeFunctions, previousFixedSize, previousChildSizeFunctions, previousRestrictions), child: DependentResource) =>
            childDependencies(visiting, current)(previousDependencySizeFunctions, previousFixedSize, previousChildSizeFunctions, previousRestrictions)(child)
        }

      if (childSizeFunctions.isEmpty)
        (totalDependencySizes + (current -> KnownDependencySize(fixedSize, offsetDirection)), restrictions ++ totalRestrictions)
      else {
        val dependencySize = (assumptions: Map[DependentResource, Int]) => fixedSize + childSizeFunctions.map(_(assumptions)).sum
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
    (visiting: Set[DependentResource],
      current: DependentResource)
    (previousDependencySizeFunctions: Map[DependentResource, DependencySize],
      previousFixedSize: Int,
      previousChildSizeFunctions: Seq[Map[DependentResource, Int] => Int],
      previousRestrictions: Map[DependentResource, Set[Int]])
    (child: DependentResource) = {

    if (visiting.contains(child))
    // cyclic dependency: add a dependency which can be resolved at a higher level
      (previousDependencySizeFunctions, previousFixedSize, previousChildSizeFunctions :+ ((assumptions: Map[DependentResource, Int]) =>
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
          val size = (assumptions: Map[DependentResource, Int]) => assumptions.getOrElse(child,
            child.sizeForDependencySize(unknown.sizeFunction(assumptions), unknown.offsetDirection))
          (newDependencySizes, previousFixedSize, previousChildSizeFunctions :+ size, newRestrictions)
      }
    }
  }
}
