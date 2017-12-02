package assembler

import assembler.sections.{LastIteration, Section}

abstract class Application protected (
  val sections: List[Section]) {

  def startOffset: Int

  lazy val encodableSections: List[Section with LastIteration] = {
    val referenceMap: Map[Reference, Encodable] = encodablesForReferences(sections.flatMap(s => s.content.collect{case r: Reference => r}))
    sections.map(s => Section.lastIteration(s.sectionType, s.name, s.content.map {
      case reference: Reference => referenceMap(reference)
      case encodable: Encodable => encodable
    }))
  }

  def getAbsoluteOffset(label: Label): Long =
    encodableSections.filter(s => s.contains(label))
      .map(s => sectionOffset(s) + s.offset(label)).head

  def sectionOffset(section: Section with LastIteration): Long

  def encodeByte: List[Byte]

  def intermediateResources(from: Reference): (List[Resource], OffsetDirection)

  private def applicationContextProperties(from: Reference): (Seq[Reference], Int, OffsetDirection) = {
    val (resources, offsetType) = intermediateResources(from)
    val (dependent: Seq[Reference], independent: Seq[Encodable]) =
      resources.partition {
        case _: Reference => true
        case _: Encodable => false
      }
    val independentDistance = offsetType match {
      case OffsetDirection.Absolute => independent.map(r => r.asInstanceOf[Encodable].size).sum + startOffset
      case _ => independent.map(r => r.asInstanceOf[Encodable].size).sum
    }
    (dependent, independentDistance, offsetType)
  }

  def encodablesForReferences(references: Seq[Reference]): Map[Reference, Encodable] = {
    val (distanceFunctions: Map[Reference, DistanceFunction], restrictions: Map[Reference, Seq[Int]]) =
      references.foldLeft((Map.empty[Reference, DistanceFunction], Map.empty[Reference, Seq[Int]])) {
        case ((
            currentDistanceFunctions: Map[Reference, DistanceFunction],
            currentRestrictions: Map[Reference, Seq[Int]]),
            currentReference: Reference) =>

          val (newDistanceFunctions, newRestrictions) =
            distanceFunctionsForDependencies(Set.empty, currentDistanceFunctions, currentRestrictions)(currentReference)

          (currentDistanceFunctions ++ newDistanceFunctions, currentRestrictions ++ newRestrictions)
      }

    val validCombinations = possibleSizeCombinations(restrictions).filter(c => c == c.map{
        case (reference, _) =>
          reference -> reference.sizeForDistance(distanceFunctions(reference).distance(c), distanceFunctions(reference).offsetDirection)
      })

    // TODO Handle the possibility that no combination is valid
    val shortestCombination = validCombinations.minBy(_.values.sum)

    references.map(reference =>
      reference -> reference.encodeForDistance(
        distanceFunctions(reference).distance(shortestCombination),
        distanceFunctions(reference).offsetDirection))
      .toMap
  }

  private def possibleSizeCombinations(references: Map[Reference, Seq[Int]]): Set[Map[Reference, Int]]  =
    if (references.isEmpty)
      Set(Map.empty)
    else
      for (
        t <- possibleSizeCombinations(references.tail);
        h <- references.head._2
      ) yield
        t + (references.head._1 -> h)

  private sealed abstract class DistanceFunction(val offsetDirection: OffsetDirection) {
    def distance(assumptions: Map[Reference, Int]): Int

    def addDistanceFunction(reference: Reference, newDistanceFunction: DistanceFunction): DistanceFunction =  newDistanceFunction match {
      case known: KnownDistance => addDistanceFunction(reference, known)
      case unknown: UnknownDistance => addDistanceFunction(reference, unknown)
    }
  }

  private case class KnownDistance(distance: Int, override val offsetDirection: OffsetDirection) extends DistanceFunction(offsetDirection) {
    override def distance(assumptions: Map[Reference, Int]): Int = distance
 }

  private case class UnknownDistance(
    distanceFunction: Map[Reference, Int] => Int,
    override val offsetDirection: OffsetDirection
  ) extends DistanceFunction(offsetDirection) {

    override def distance(assumptions: Map[Reference, Int]): Int = distanceFunction(assumptions)
 }

  private final def distanceFunctionsForDependencies(visiting: Set[Reference],
    visited: Map[Reference, DistanceFunction], restrictions: Map[Reference, Seq[Int]])(current: Reference):
      (Map[Reference, DistanceFunction], Map[Reference, Seq[Int]]) = {

    if (visited.contains(current))
      // this reference has been evaluated in an earlier call (in a prior branch)
      (Map.empty, Map.empty)
    else {
      val (references, independentDistance, offsetDirection) = applicationContextProperties(current)

      val (distanceFunctions, fixedDistance, childDistanceFunctions, totalRestrictions) =
        references.foldLeft[(Map[Reference, DistanceFunction], Int, Seq[Map[Reference, Int] => Int], Map[Reference, Seq[Int]])](
          (visited, independentDistance, Seq.empty, Map.empty)
        ) {
          case (
            (previousDistanceFunctions, previousFixedDistance, previousChildDistanceFunctions, previousRestrictions), child: Resource) =>

            if (visiting.contains(child))
              // cyclic dependency: add a dependency which can be resolved at a higher level
              (previousDistanceFunctions, previousFixedDistance, previousChildDistanceFunctions :+ ((assumptions: Map[Reference, Int]) => assumptions(child)), previousRestrictions + (child -> child.possibleSizes))
            else {
              val (childDistanceFunctions, childRestrictions) = distanceFunctionsForDependencies(visiting + current, previousDistanceFunctions, previousRestrictions)(child)
              val newDistanceFunctions = previousDistanceFunctions ++ childDistanceFunctions
              val newRestrictions = previousRestrictions ++ childRestrictions
              val distanceFunction = newDistanceFunctions(child)
              distanceFunction match {
                case known: KnownDistance =>
                  val size = child.sizeForDistance(known.distance, known.offsetDirection)
                  (newDistanceFunctions, previousFixedDistance + size, previousChildDistanceFunctions, newRestrictions)
                case unknown: UnknownDistance =>
                  val size = (assumptions: Map[Reference, Int]) => child.sizeForDistance(unknown.distanceFunction(assumptions), unknown.offsetDirection)
                  (newDistanceFunctions, previousFixedDistance, previousChildDistanceFunctions :+ size, newRestrictions)
              }
            }
        }

      val totalDistanceFunction = if (childDistanceFunctions.isEmpty)
        KnownDistance(fixedDistance, offsetDirection)
      else
        UnknownDistance((assumptions) => independentDistance + childDistanceFunctions.map(f => f(assumptions)).sum, offsetDirection)

      //TODO: further restrict the restrictions list here
      (distanceFunctions + (current -> totalDistanceFunction), restrictions ++ totalRestrictions)
    }
  }
}
