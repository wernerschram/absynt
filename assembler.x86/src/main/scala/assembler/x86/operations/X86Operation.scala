package assembler.x86.operations

import assembler.resource.UnlabeledEncodable
import assembler.x86.operands._
import assembler.x86.{ProcessorMode, RexRequirement}


abstract class X86Operation extends UnlabeledEncodable {
  val includeRexW: Boolean = true

  def operands: Seq[OperandInfo]

  override def size: Int = encodeByte.length

  override def encodeByte: Seq[Byte] = {
    validate()

    optionalSegmentOverridePrefix ++
      optionalAddressSizePrefix ++
      optionalOperandSizePrefix ++
      optionalRexPrefix ++
      code
  }

  implicit val processorMode: ProcessorMode

  def validate(): Unit = Unit

  private def optionalSegmentOverridePrefix: List[Byte] = segmentOverride match {
    case Some(segment) => X86Operation.SegmentOverrideMap.get(segment).toList
    case _ => Nil
  }

  def segmentOverride: Option[SegmentRegister] = None

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

  def rexRequirements: Seq[RexRequirement] =
    operands.flatMap(o => o.rexRequirements ++ o.addressOperands.flatMap(_.rexRequirements))

  def code: Seq[Byte]

  def mnemonic: String

  override def toString: String = if (operands.isEmpty)
      s"$mnemonic"
    else
      s"$mnemonic ${operands.sorted.map{ _.toString }.mkString(", ")}"
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
