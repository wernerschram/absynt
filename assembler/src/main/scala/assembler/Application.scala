package assembler

import assembler.sections.{LastIteration, Section}

abstract class Application[OffsetType<:Offset, AddressType<:Address[OffsetType]] protected (
  val sections: List[Section[OffsetType]])
  (implicit offsetFactory: OffsetFactory[OffsetType], addressFactory: AddressFactory[OffsetType, AddressType]) {

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

  def estimateAbsoluteAddress(label: Label): Estimate[AddressType] =
    sections.filter(s => s.contains(label))
      .map(s => s.estimatedOffset(label).map(v => addressFactory.add(memoryAddress(s), v))).head

  def encodeByte: List[Byte]


  def intermediateResources(from: Reference): List[Resource]

  private def intermediateReferencesAndOther(from: Reference): (Seq[Reference], Seq[Encodable]) = {
    val (dependent: Seq[Reference], independent: Seq[Encodable]) =
      intermediateResources(from).partition {
        case _: Reference => true
        case _: Encodable => false
      }
    (dependent, independent)
  }

  def encodablesForReferences(references: Seq[Reference]): Map[Reference, Encodable] = {
    val sizeFunctions: Map[Reference, SizeFunction] =
      references.foldLeft(Map.empty[Reference, SizeFunction])((x,y) => x ++ sizeFunctionsForDependencies(Nil, x)(y))

    sizeFunctions.foldLeft(Map.empty[Reference, Encodable])((resources, resourceSize) => {
      val todo: Seq[Reference] = resourceSize._2.requiredAssumptions.filterNot(resources.contains)
      val validCombinations = possibleSizeCombinations(todo).filter(c => !c.exists{case (reference, value) => sizeFunctions(reference).size(c) != value})

      resources + (resourceSize._1 -> resourceSize._1.encodeForDistance(resourceSize._2.size(validCombinations.head)))
    })
  }

  private def possibleSizeCombinations(references: Seq[Reference]): Seq[Map[Reference, Int]]  =
    if (references.isEmpty)
      Seq(Map.empty[Reference,Int])
    else
      for (
        t <- possibleSizeCombinations(references.tail);
        h <- references.head.possibleSizes
      ) yield
        t + (references.head -> h)

  private type sizeForAssumptions = Map[Reference, Int] => Int
  private type distanceForAssumptions = Map[Reference, Int] => Int

  private case class SizeFunction(requiredAssumptions: Seq[Reference], size: sizeForAssumptions)

  private case class IncrementalDistance(requiredAssumptions: Seq[Reference], distance: distanceForAssumptions)

  private final def sizeFunctionsForDependencies(visiting: List[Reference], visited: Map[Reference, SizeFunction])(start: Reference): Map[Reference, SizeFunction] = {
    val (references, resources) = intermediateReferencesAndOther(start)
    val independentDistance = resources.map(r => r.size).sum
    if (visited.contains(start)) {
      // this branch that has been evaluated in an earlier call (in a neighbour branch)
      visited
    } else if (visiting.contains(start)) {
      // cyclic dependency: add a required assumption so that the caller can resolve the dependency
      visited + (start -> SizeFunction(start:: Nil, assumptions => assumptions(start)))
    }
    else {
      val (sizeFunctions: Map[Reference, SizeFunction], distance: IncrementalDistance) = references.foldLeft(
        (visited, IncrementalDistance(Seq.empty[Reference], (_: Map[Reference, Int]) => independentDistance))
      ){
        case ((previousSizeFunctions: Map[Reference, SizeFunction], previousDistance: IncrementalDistance), current) =>
          val evaluatedSizeFunctions: Map[Reference, SizeFunction] = sizeFunctionsForDependencies(start::visiting, previousSizeFunctions)(current)
          (previousSizeFunctions ++ evaluatedSizeFunctions, incrementalDistanceForAssumption(previousDistance, evaluatedSizeFunctions(current)))
      }

      val requiredAssumptions = distance.requiredAssumptions
      def size(assumptions: Map[Reference, Int]) = start.sizeForDistance(distance.distance(assumptions))

      // try to remove the required assumptions
      val possibleSizes = possibleSizeCombinations(requiredAssumptions).map(c => size(c))
      if (possibleSizes.size == 1) {
        val actualSize = possibleSizes.head // evaluate before assigning to reduce lambda nesting
          sizeFunctions + (start -> SizeFunction(Nil, (_: Map[Reference, Int]) => actualSize))
      }
      else
        sizeFunctions + (start -> SizeFunction(requiredAssumptions, size))
    }
  }

  private def incrementalDistanceForAssumption(previousDistance: IncrementalDistance, newSizeFunction: SizeFunction): IncrementalDistance = {
    val requiredAssumptions = previousDistance.requiredAssumptions ++ newSizeFunction.requiredAssumptions
    (previousDistance.requiredAssumptions, newSizeFunction.requiredAssumptions) match {
      case (Nil, Nil) =>
        val value = previousDistance.distance(Map.empty[Reference, Int]) + newSizeFunction.size(Map.empty[Reference, Int])
        IncrementalDistance(requiredAssumptions, (_: Map[Reference, Int]) => value)
      case (Nil, _) =>
        val value = previousDistance.distance(Map.empty[Reference, Int])
        IncrementalDistance(requiredAssumptions, (assumptions: Map[Reference, Int]) => value + newSizeFunction.size(assumptions))
      case (_, Nil) =>
        val value = newSizeFunction.size(Map.empty[Reference, Int])
        IncrementalDistance(requiredAssumptions, (assumptions: Map[Reference, Int]) => previousDistance.distance(assumptions) + value)
      case (_, _) =>
        IncrementalDistance(requiredAssumptions, (assumptions: Map[Reference, Int]) => previousDistance.distance(assumptions) + newSizeFunction.size(assumptions))
    }
  }
}
