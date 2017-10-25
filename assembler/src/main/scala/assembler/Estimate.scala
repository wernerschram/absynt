package assembler

import scala.runtime.Nothing$

sealed trait Estimate[+V] {
  def tempMinimum: V
  def tempMaximum: V
  def map[T](operation: (V) => T): Estimate[T]
}

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

case class Bounded[V](minimum: V, maximum: V) extends Estimate[V] {
  val tempMinimum: V = minimum
  val tempMaximum: V = maximum
  def map[T](operation: (V) => T): Estimate[T] = Bounded(operation(minimum), operation(maximum))
}

case class Actual[V](value: V) extends Estimate[V] {
  val tempMinimum: V = value
  val tempMaximum: V = value
  def map[T](operation: (V) => T): Estimate[T] = Actual(operation(value))
}

case object Unknown extends Estimate[Nothing] {
  def tempMinimum: Nothing = ???
  def tempMaximum: Nothing = ???
  def map[T](operation: (Nothing) => T): Estimate[T] = Unknown
}
