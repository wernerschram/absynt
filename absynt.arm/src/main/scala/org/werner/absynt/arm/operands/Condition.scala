package org.werner.absynt.arm.operands

object Condition {

  sealed abstract class Condition(val value: Byte, val mnemonicExtension: String)

  case object Equal extends Condition(0x0, "eq")
  case object ZeroSet extends Condition(0x0, "zs")
  case object NotEqual extends Condition(0x1, "ne")
  case object ZeroClear extends Condition(0x1, "zc")
  case object CarrySet extends Condition(0x2, "cs")
  case object UnsignedHigherOrSame extends Condition(0x2, "hs")
  case object CarryClear extends Condition(0x3, "cc")
  case object UnsignedLower extends Condition(0x3, "lo")
  case object Minus extends Condition(0x4, "mi")
  case object NegativeSet extends Condition(0x4, "ns")
  case object Plus extends Condition(0x5, "pl")
  case object NegativeClear extends Condition(0x5, "nc")
  case object Overflow extends Condition(0x6, "vs")
  case object NoOverflow extends Condition(0x7, "vc")
  case object UnsignedHigher extends Condition(0x8, "hi")
  case object LowerOrSame extends Condition(0x9, "ls")
  case object SignedGreaterOrEqual extends Condition(0xa, "ge")
  case object SignedLessThan extends Condition(0xb, "lt")
  case object SignedGreaterThan extends Condition(0xc, "gt")
  case object SignedLessOrEqual extends Condition(0xd, "le")
  case object Always extends Condition(0xe, "")
  case object Unpredictable extends Condition(0xf, "")

}