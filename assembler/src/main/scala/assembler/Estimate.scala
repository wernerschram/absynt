package assembler

sealed trait Estimate[+V]

object Estimate {
  def apply[V](minimum: V, maximum: V): Estimate[V] = {
    if (minimum == maximum)
      Actual(minimum)
    else
      Bounded(minimum, maximum)
  }

  private def reduceInnerBounded[V](operation: (V, V) => V)(bounded1: Bounded[V], value: Estimate[V]): Estimate[V] = value match {
    case bounded2: Bounded[V] => Bounded(operation(bounded1.minimum, bounded2.minimum), operation(bounded1.maximum, bounded2.maximum))
    case actual2: Actual[V] => Bounded(operation(bounded1.minimum, actual2.value), operation(bounded1.maximum, actual2.value))
    case _ => Unknown
  }

  def reduceInner[V](operation: (V, V) => V)(estimate1: Estimate[V], estimate2: Estimate[V]): Estimate[V] = (estimate1, estimate2) match {
    case (bounded1: Bounded[V], _) => reduceInnerBounded(operation)(bounded1, estimate2)
    case (_, bounded2: Bounded[V]) => reduceInnerBounded(operation)(bounded2, estimate1)
    case (actual1: Actual[V], actual2: Actual[V]) => Actual(operation(actual1.value, actual2.value))
    case (_, _) => Unknown
  }
}

case class Bounded[V](minimum: V, maximum: V) extends Estimate[V]

case class Actual[V](value: V) extends Estimate[V]

case object Unknown extends Estimate[Nothing]
