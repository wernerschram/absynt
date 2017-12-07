package assembler

import assembler.reference.{AbsoluteReference, RelativeReference}
import assembler.sections.{AlignmentFiller, LastIteration, Section}

abstract class Application protected (
  val sections: List[Section]) {

  def startOffset: Int

  lazy val encodableSections: List[Section with LastIteration] = {
    val dependentMap: Map[DependentResource, Encodable] = encodablesForReferences(sections.flatMap(s => s.content.collect{case r: DependentResource => r}))
    sections.map(s => Section.lastIteration(s.sectionType, s.name, s.content.map {
      case reference: DependentResource => dependentMap(reference)
      case encodable: Encodable => encodable
    }))
  }

  def getAbsoluteOffset(label: Label): Long =
    encodableSections.filter(s => s.contains(label))
      .map(s => sectionOffset(s) + s.offset(label)).head

  def sectionOffset(section: Section with LastIteration): Long =
    encodableSections.takeWhile(s => s != section).map(_.size).sum + startOffset

  def encodeByte: List[Byte]

  def intermediateResources(from: DependentResource): (List[Resource], OffsetDirection) = from match {
    case relative: RelativeReference =>
      val section = sections.filter(s => s.contains(from)).head
      (section.intermediateResources(relative), section.offsetDirection(relative))
    case absolute: AbsoluteReference => (
      sections.takeWhile(s => !s.contains(absolute.target)).flatMap(s => s.content) ++
      sections.filter(s => s.contains(absolute.target)).head.content.takeWhile(r => r.label != absolute.target), OffsetDirection.Absolute
      )
    case alignment: AlignmentFiller =>
      (sections.takeWhile(s => s != alignment.section).flatMap(s => s.content), OffsetDirection.Absolute)
  }

  private def applicationContextProperties(from: DependentResource): (Seq[DependentResource], Int, OffsetDirection) = {
    val (resources, offsetType) = intermediateResources(from)

    val (totalDependent, totalIndependent) = resources
      .foldLeft((Seq.empty[DependentResource], 0))
      {
        case ((dependent, independent), reference: DependentResource) => (dependent :+ reference, independent)
        case ((dependent, independent), encodable: Encodable) => (dependent, independent + encodable.size)
      }

    offsetType match {
      case OffsetDirection.Absolute =>
        (totalDependent, totalIndependent + startOffset, offsetType)
      case _ =>
        (totalDependent, totalIndependent, offsetType)
    }
  }

  def encodablesForReferences(references: Seq[DependentResource]): Map[DependentResource, Encodable] = {
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
      reference -> reference.encodeForDependencySize(
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
        h <- references.head._2
      ) yield
        t + (references.head._1 -> h)

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
      val (references, independentSizes, offsetDirection) = applicationContextProperties(current)

      val (totalDependencySizes, fixedSize, childSizeFunctions, totalRestrictions) =
        references.foldLeft((visited, independentSizes, Seq.empty[Map[DependentResource, Int] => Int], Map.empty[DependentResource, Set[Int]])) {
          case ((previousDependencySizeFunctions, previousFixedSize, previousChildSizeFunctions, previousRestrictions), child: Resource) =>
            childDependencies(visiting, current)(previousDependencySizeFunctions, previousFixedSize, previousChildSizeFunctions, previousRestrictions)(child)
        }

      if (childSizeFunctions.isEmpty)
        (totalDependencySizes + (current -> KnownDependencySize(fixedSize, offsetDirection)), restrictions ++ totalRestrictions)
      else {
        val dependencySize = (assumptions: Map[DependentResource, Int]) => independentSizes + childSizeFunctions.map(_(assumptions)).sum
        if (totalRestrictions.contains(current)) {
          val combinations = possibleSizeCombinations(totalRestrictions)
          val sizes: Set[Int] = combinations.map(c => current.sizeForDependencySize(dependencySize(c), offsetDirection))
          (totalDependencySizes + (current -> UnknownDependencySize(dependencySize, offsetDirection)), restrictions ++ totalRestrictions.updated(current, sizes))
        } else
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
