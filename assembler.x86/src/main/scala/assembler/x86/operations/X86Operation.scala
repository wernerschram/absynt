package assembler.x86.operations

import assembler.resource.UnlabeledEncodable
import assembler.x86.operands._
import assembler.x86.{ProcessorMode, RexRequirement}

object EncodingPosition extends Enumeration {
  type EncodingPosition = Value
  val opcode, modRM, SIB, displacement, immediate = Value
}

trait ModRMBytes {
  self: X86Operation =>
  override def modRMBytes: Seq[Byte]
}

trait DisplacementBytes {
  self: X86Operation =>
  override def displacementBytes: Seq[Byte]
}

trait ImmediateBytes {
  self: X86Operation =>
  override def immediateBytes: Seq[Byte]
}

trait NoModRM extends ModRMBytes {
  self: X86Operation =>
  override def modRMBytes: Seq[Byte] = Nil
}

trait NoDisplacement extends DisplacementBytes {
  self: X86Operation =>
  override def displacementBytes: Seq[Byte] = Nil
}

trait NoImmediate extends ImmediateBytes {
  self: X86Operation =>
  override def immediateBytes: Seq[Byte] = Nil
}

abstract class X86Operation(val code: Seq[Byte])(implicit val processorMode: ProcessorMode) extends UnlabeledEncodable {
  self: ModRMBytes with DisplacementBytes with ImmediateBytes =>
  final def prefixes: Seq[Byte] =
    optionalSegmentOverridePrefix ++
      optionalAddressSizePrefix ++
      optionalOperandSizePrefix ++
      optionalRexPrefix

  def modRMBytes: Seq[Byte]
  def displacementBytes: Seq[Byte]
  def immediateBytes: Seq[Byte]

  def operands: Set[OperandInfo]

  override def size: Int = encodeByte.length

  override def encodeByte: Seq[Byte] = {
    assert(operands.forall(o => o.operand.isValidForMode(processorMode)))
    prefixes ++
      code ++
      modRMBytes ++
      displacementBytes ++
      immediateBytes
  }

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
    if (operandString.isEmpty)
      s"$mnemonic"
    else
      s"$mnemonic $operandString"
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
