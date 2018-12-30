package assembler.x86.operations

import assembler.resource.UnlabeledEncodable
import assembler.x86.operands._
import assembler.x86.{ProcessorMode, RexRequirement}

trait ModRMBytes {
  self: X86Operation =>
  def modRMBytes: Seq[Byte]
  protected def modRMInit(): Unit
}

trait DisplacementBytes {
  self: X86Operation =>
  def displacementBytes: Seq[Byte]
  protected def displacementInit(): Unit
}

trait ImmediateBytes {
  self: X86Operation =>
  def immediateBytes: Seq[Byte]
  protected def immediateInit(): Unit
}

trait NoModRM extends ModRMBytes {
  self: X86Operation =>
  override def modRMBytes: Seq[Byte] = Nil
  protected def modRMInit(): Unit = Unit
}

trait NoDisplacement extends DisplacementBytes {
  self: X86Operation =>
  override def displacementBytes: Seq[Byte] = Nil
  protected def displacementInit(): Unit = Unit
}

trait NoImmediate extends ImmediateBytes {
  self: X86Operation =>
  override def immediateBytes: Seq[Byte] = Nil
  protected def immediateInit(): Unit = Unit
}

abstract class X86Operation(val code: Seq[Byte])(implicit val processorMode: ProcessorMode) extends UnlabeledEncodable {
  self: ModRMBytes with DisplacementBytes with ImmediateBytes =>
  final def prefixes: Seq[Byte] =
    optionalRepeatPrefix ++
      optionalSegmentOverridePrefix ++
      optionalAddressSizePrefix ++
      optionalOperandSizePrefix ++
      optionalRexPrefix

  protected def implicitInit(): Unit = Unit

  private final val operandsBuilder = Set.newBuilder[OperandInfo]

  protected final def addOperand(operand: OperandInfo): Unit = operandsBuilder += operand

  final lazy val operands: Set[OperandInfo] = {
    implicitInit()
    modRMInit()
    displacementInit()
    immediateInit()
    operandsBuilder.result()
  }

  override def size: Int = encodeByte.length

  override def encodeByte: Seq[Byte] = {
    assert(operands.forall(o => o.operand.isValidForMode(processorMode)))
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
    if (operands.flatMap(_.addressOperands).exists(_.requiresAddressSize(processorMode))) X86Operation.AddressSizeCode :: Nil else Nil

  private def optionalOperandSizePrefix: List[Byte] =
    if (operands.exists(_.requiresOperandSize(processorMode))) X86Operation.OperandSizeCode :: Nil else Nil

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
    (Register.CS, 0x2E.toByte),
    (Register.SS, 0x36.toByte),
    (Register.DS, 0x3E.toByte),
    (Register.ES, 0x26.toByte),
    (Register.FS, 0x64.toByte),
    (Register.GS, 0x65.toByte))
}
