package assembler

import assembler.sections.{LastIteration, Section}

abstract class Application[OffsetType<:Offset] protected (
  val sections: List[Section[OffsetType]])
  (implicit offsetFactory: OffsetFactory[OffsetType]) {

  def startOffset: Int

  lazy val encodableSections: List[Section[OffsetType] with LastIteration[OffsetType]] = {
    val referenceMap: Map[Reference, Encodable] = encodablesForReferences(sections.flatMap(s => s.content.collect{case r: Reference => r}))
    sections.map(s => Section.lastIteration(s.sectionType, s.name, s.content.map {
      case reference: Reference => referenceMap(reference)
      case encodable: Encodable => encodable
    }))
  }

  def getAbsoluteOffset(label: Label): Long =
    encodableSections.filter(s => s.contains(label))
      .map(s => sectionOffset(s) + s.offset(label)).head

  def sectionOffset(section: Section[OffsetType] with LastIteration[OffsetType]): Long

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
    val distanceFunctions: Map[Reference, DistanceFunction] =
      references.foldLeft(Map.empty[Reference, DistanceFunction])((x,y) => x ++ distanceFunctionsForDependencies(Set.empty, x)(y))

    distanceFunctions.foldLeft(Map.empty[Reference, Encodable])((resources, resourceSize) => {
      val todo: Map[Reference, Seq[Int]] = resourceSize._2.requiredAssumptions.map(reference => resources.get(reference) match {
        case None => reference -> reference.possibleSizes
        case Some(resource) => reference -> Seq(resource.size)
      }).toMap

      val validCombinations = possibleSizeCombinations(todo).filter(c => c == c.map{
        case (reference, _) =>
          reference -> reference.sizeForDistance(distanceFunctions(reference).distance(c), distanceFunctions(reference).offsetDirection)
      })

      val shortestCombination = validCombinations.minBy(_.values.sum)

      resources + (resourceSize._1 -> resourceSize._1.encodeForDistance(resourceSize._2.distance(shortestCombination), resourceSize._2.offsetDirection))
    })
  }

  private def possibleSizeCombinations(references: Map[Reference, Seq[Int]]): Set[Map[Reference, Int]]  =
    if (references.isEmpty)
      Set(Map.empty[Reference,Int])
    else
      for (
        t <- possibleSizeCombinations(references.tail);
        h <- references.head._2
      ) yield
        t + (references.head._1 -> h)

  private case class DistanceFunction(
    requiredAssumptions: Set[Reference],
    distance: Map[Reference, Int] => Int,
    offsetDirection: OffsetDirection)

  private final def distanceFunctionsForDependencies(visiting: Set[Reference],
    visited: Map[Reference, DistanceFunction])(current: Reference): Map[Reference, DistanceFunction] = {

    if (visited.contains(current))
      // this reference that has been evaluated in an earlier call (in a prior branch)
      Map.empty[Reference, DistanceFunction]
    else {
      val (references, independentDistance, offsetDirection) = applicationContextProperties(current)

      val (distanceFunctions: Map[Reference, DistanceFunction], newDistanceFunction: DistanceFunction) =
        references.foldLeft(
          (visited,
            DistanceFunction(Set.empty[Reference],
            (_: Map[Reference, Int]) => independentDistance, offsetDirection))
        ) {
          case (
            (previousDistanceFunctions: Map[Reference, DistanceFunction],
            previousDistance: DistanceFunction),
            child: Resource) =>

            if (visiting.contains(child))
              // cyclic dependency: add a dependency which can be resolved at a higher level
              (previousDistanceFunctions,
                DistanceFunction(previousDistance.requiredAssumptions + child,
                  (assumptions) => previousDistance.distance(assumptions) + assumptions(child), offsetDirection))
            else {
              val evaluatedDistanceFunctions: Map[Reference, DistanceFunction] =
                distanceFunctionsForDependencies(visiting + current, previousDistanceFunctions)(child)

              val newDistanceFunctions = previousDistanceFunctions ++ evaluatedDistanceFunctions
              val childDistanceFunction: DistanceFunction = newDistanceFunctions(child)

              (newDistanceFunctions,
                incrementalDistanceForAssumption(child, previousDistance, childDistanceFunction))
            }
        }
      distanceFunctions + (current -> newDistanceFunction)

      // try to remove the required assumptions

//      val sizeForCombination = possibleSizeCombinations(requiredAssumptions).map(c => c -> current.sizeForDistance(distance.distance(c)))
//      if (sizeForCombination.groupBy(s => s._2).size == 1) {
//        val actualDistance = distance.distance(sizeForCombination.map(s=>s._1).head) // evaluate before assigning to reduce lambda nesting
//        distanceFunctions + (current -> DistanceFunction(Nil, (_: Map[Reference, Int]) => actualDistance))
//      }
//      else
//        distanceFunctions + (current -> distance)
    }
  }

  private def incrementalDistanceForAssumption(
    reference: Reference,
    previousDistance: DistanceFunction,
    newDistanceFunction: DistanceFunction): DistanceFunction = {
    val requiredAssumptions = previousDistance.requiredAssumptions ++ newDistanceFunction.requiredAssumptions
    (previousDistance.requiredAssumptions.toSeq, newDistanceFunction.requiredAssumptions.toSeq) match {
      case (Nil, Nil) =>
        val value = previousDistance.distance(Map.empty[Reference, Int]) +
          reference.sizeForDistance(newDistanceFunction.distance(Map.empty[Reference, Int]), previousDistance.offsetDirection)
        DistanceFunction(requiredAssumptions, (_: Map[Reference, Int]) => value, previousDistance.offsetDirection)
      case (Nil, _) =>
        val value = previousDistance.distance(Map.empty[Reference, Int])
        DistanceFunction(requiredAssumptions, (assumptions: Map[Reference, Int]) =>
          value + reference.sizeForDistance(newDistanceFunction.distance(assumptions), newDistanceFunction.offsetDirection), previousDistance.offsetDirection)
      case (_, Nil) =>
        val value = reference.sizeForDistance(newDistanceFunction.distance(Map.empty[Reference, Int]), newDistanceFunction.offsetDirection)
        DistanceFunction(requiredAssumptions, (assumptions: Map[Reference, Int]) =>
          previousDistance.distance(assumptions) + value, previousDistance.offsetDirection)
      case (_, _) =>
        DistanceFunction(requiredAssumptions, (assumptions: Map[Reference, Int]) =>
          previousDistance.distance(assumptions) + reference.sizeForDistance(newDistanceFunction.distance(assumptions), newDistanceFunction.offsetDirection), previousDistance.offsetDirection)
    }
  }
}
