package assembler

import assembler.sections.{LastIteration, Section}

abstract class Application[OffsetType<:Offset, AddressType<:Address[OffsetType]] protected (
  val sections: List[Section[OffsetType]])
  (implicit offsetFactory: OffsetFactory[OffsetType], addressFactory: AddressFactory[OffsetType, AddressType]) {

  def startOffset: Int

  lazy val encodableSections: List[Section[OffsetType] with LastIteration[OffsetType]] = {
    val referenceMap: Map[Reference, Encodable] = encodablesForReferences(sections.flatMap(s => s.content.collect{case r: Reference => r}))
    sections.map(s => Section.lastIteration(s.sectionType, s.name, s.content.map {
      case reference: Reference => referenceMap(reference)
      case encodable: Encodable => encodable
    }))
  }

  def getAbsoluteAddress(encodable: Resource): AddressType =
    encodableSections.filter(s=> s.contains(encodable))
      .map(s => addressFactory.add(memoryAddress(s), s.offset(encodable))).head

  def getAbsoluteAddress(label: Label): AddressType =
    encodableSections.filter(s => s.contains(label))
      .map(s => addressFactory.add(memoryAddress(s), s.offset(label))).head

  def memoryAddress(section: Section[OffsetType]): AddressType

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
    val sizeFunctions: Map[Reference, DistanceFunction] =
      references.foldLeft(Map.empty[Reference, DistanceFunction])((x,y) => x ++ distanceFunctionsForDependencies(Nil, x)(y))

    sizeFunctions.foldLeft(Map.empty[Reference, Encodable])((resources, resourceSize) => {
      val todo: Set[Reference] = resourceSize._2.requiredAssumptions.filterNot(resources.contains)
      val validCombinations = possibleSizeCombinations(todo).filter(c => !c.exists{
        case (reference, value) => {
          reference.sizeForDistance(sizeFunctions(reference).distance(c), sizeFunctions(reference).offsetDirection) != value
        }
      })

      //TODO !!!
      resources + (resourceSize._1 -> resourceSize._1.encodeForDistance(resourceSize._2.distance(validCombinations.head), resourceSize._2.offsetDirection))
    })
  }

  private def possibleSizeCombinations(references: Set[Reference]): Seq[Map[Reference, Int]]  =
    if (references.isEmpty)
      Seq(Map.empty[Reference,Int])
    else
      for (
        t <- possibleSizeCombinations(references.tail);
        h <- references.head.possibleSizes
      ) yield
        t + (references.head -> h)

  private type distanceForAssumptions = Map[Reference, Int] => Int

  private case class DistanceFunction(
    requiredAssumptions: Set[Reference],
    distance: distanceForAssumptions,
    offsetDirection: OffsetDirection)

  private final def distanceFunctionsForDependencies(visiting: List[Reference],
    visited: Map[Reference, DistanceFunction])(current: Reference): Map[Reference, DistanceFunction] = {

    if (visited.contains(current))
      // this reference that has been evaluated in an earlier call (in a prior branch)
      visited
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
                distanceFunctionsForDependencies(current :: visiting, previousDistanceFunctions)(child)

              val childDistanceFunction: DistanceFunction = evaluatedDistanceFunctions(child)

              (previousDistanceFunctions ++ evaluatedDistanceFunctions,
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
