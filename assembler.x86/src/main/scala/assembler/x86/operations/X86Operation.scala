package assembler.x86.operations

import assembler.resource.UnlabeledEncodable
import assembler.x86.operands._
import assembler.x86.{ProcessorMode, RexRequirement}

abstract class X86Operation(val code: Seq[Byte])(implicit val processorMode: ProcessorMode) extends UnlabeledEncodable {
  self: ModRMBytes with DisplacementBytes with ImmediateBytes =>
  final def prefixes: Seq[Byte] =
    optionalRepeatPrefix ++
      optionalSegmentOverridePrefix ++
      optionalAddressSizePrefix ++
      optionalOperandSizePrefix ++
      optionalRexPrefix

  protected def implicitInit(): Unit = Unit

  private final val operandsBuilder = Set.newBuilder[OperandInfo[_]]

  protected final def addOperand(operand: OperandInfo[_]): Unit = operandsBuilder += operand

  final lazy val operands: Set[OperandInfo[_]] = {
    implicitInit()
    modRMInit()
    displacementInit()
    immediateInit()
    operandsBuilder.result()
  }

  override def size: Int = encodeByte.length

  override def encodeByte: Seq[Byte] = {
    prefixes ++
      code ++
      modRMBytes ++
      displacementBytes ++
      immediateBytes
  }

  protected def repeated: Boolean = false

  private def optionalRepeatPrefix: List[Byte] =
    if (repeated) 0xF3.toByte :: Nil else Nil

  private def optionalSegmentOverridePrefix: List[Byte] =
      operands.flatMap(_.addressOperands).flatMap(_.segmentOverride).flatMap(X86Operation.SegmentOverrideMap.get).toList

  private def optionalAddressSizePrefix: List[Byte] =
    if (operands.flatMap(_.addressOperands).exists(_.requiresAddressSize)) X86Operation.AddressSizeCode :: Nil else Nil

  private def optionalOperandSizePrefix: List[Byte] =
    if (operands.exists(_.requiresOperandSize)) X86Operation.OperandSizeCode :: Nil else Nil

  private def optionalRexPrefix: List[Byte] = {
    assume(processorMode == ProcessorMode.Long || rexRequirements.isEmpty)
    if (rexRequirements.isEmpty)
      Nil
    else
      rexRequirements.foldLeft[Byte](X86Operation.RexCode)((value, req) => (value | req.rexBitMask).toByte) :: Nil
  }

  lazy val rexRequirements: Set[RexRequirement] =
    operands.flatMap(o => o.rexRequirements ++ o.addressOperands.flatMap(_.rexRequirements))


  def mnemonic: String

  override def toString: String = {
    val operandString = operands.toSeq.sorted.map(_.toString).mkString(", ")
    s"${if (repeated) "rep " else ""}$mnemonic${if (operandString.nonEmpty) s" $operandString" else ""}"
  }
}

object X86Operation {
  private val RexCode = 0x40.toByte

  private val OperandSizeCode = 0x66.toByte
  private val AddressSizeCode = 0x67.toByte

  private val SegmentOverrideMap: Map[SegmentRegister, Byte] = Map(
    (Segment.Code, 0x2E.toByte),
    (Segment.Stack, 0x36.toByte),
    (Segment.Data, 0x3E.toByte),
    (Segment.Extra, 0x26.toByte),
    (Segment.MoreExtra, 0x64.toByte),
    (Segment.StillMoreExtra, 0x65.toByte))
}
