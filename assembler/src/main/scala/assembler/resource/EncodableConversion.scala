package assembler.resource

import assembler.Label

object EncodableConversion {
  implicit class Resources(resources: Seq[Resource]) {
    def encodables(dependentMap: Map[DependentResource, Encodable]): Seq[Encodable] = resources.map {
      case reference: DependentResource => dependentMap(reference)
      case encodable: Encodable => encodable
    }

    def dependentResources: Seq[DependentResource] = resources.collect{case r: DependentResource => r}

    def containsLabel(label: Label): Boolean =
      resources.collect{ case r: Labeled => r}.exists(_.label.matches(label))
  }

  implicit class Encodables(encodables: Seq[Encodable]) {
    def encodeByte: Seq[Byte] = encodables.flatMap { x => x.encodeByte }
  }
}
