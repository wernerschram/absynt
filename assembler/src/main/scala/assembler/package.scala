import scala.language.implicitConversions

/**
  * Created by werners on 24-3-2017.
  */
package object assembler {
  implicit def apply[TargetType >: Encodable](t: TargetType): Designation[TargetType] = new Unlabeled[TargetType](t)
}
