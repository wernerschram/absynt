package assembler.x86.operands.memoryaccess

import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.operands.FixedSizeModRMEncodableOperand
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.registers._

sealed class SIBMemoryLocation(val index: SIBIndexRegister, val base: SIBBaseRegister, displacement: List[Byte], val scale: Int, segment: SegmentRegister)
    extends IndirectMemoryLocation(0x04, displacement, index.operandByteSize, segment) with ModRMEncodableOperand {
  assume(index.operandByteSize == base.operandByteSize)
  assume((1::2::4::8::Nil).contains(scale))
  
  val baseCode = base.SIBBaseCode
  val indexCode = index.SIBIndexCode
  override val defaultSegment: SegmentRegister = index.defaultSIBSegment
    
  def getSIB: Byte = {
    val scaleCode = scale match {
      case 1 => 0x0
      case 2 => 0x1
      case 4 => 0x2
      case 8 => 0x3
      case _ => throw new Exception // TODO: replace with correct exception
    }
    ((scaleCode << 6) | (indexCode << 3) | baseCode).toByte
  }

  override def getExtendedBytes(rValue: Byte): List[Byte] = getModRM(rValue) :: getSIB :: displacement

  override def getRexRequirements(position: ParameterPosition) =
      base.getRexRequirements(ParameterPosition.Base) :::
      index.getRexRequirements(ParameterPosition.Index)

  override def isValidForMode(processorMode: ProcessorMode): Boolean = base.isValidForMode(processorMode) && index.isValidForMode(processorMode)
}

object SIBMemoryLocation {
  final class FixedSizeSIBMemoryLocation private (index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte], scale: Int, val operandByteSize: Int, segment: SegmentRegister)
      extends SIBMemoryLocation(index, base, displacement, scale, segment) with FixedSizeModRMEncodableOperand 

  private object FixedSizeSIBMemoryLocation {
    def apply(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte], scale: Int, operandByteSize: Int, segment: SegmentRegister) = 
      new FixedSizeSIBMemoryLocation(index, base, displacement, scale, operandByteSize, segment)
  }
      
  def apply(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1) = 
    new SIBMemoryLocation(index, base, displacement, scale, index.defaultSIBSegment)
  
  def byteSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1) =
    FixedSizeSIBMemoryLocation(index, base, displacement, scale, 1, index.defaultSIBSegment)

  def wordSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1) =
    FixedSizeSIBMemoryLocation(index, base, displacement, scale, 2, index.defaultSIBSegment)
  
  def doubleWordSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1) =
    FixedSizeSIBMemoryLocation(index, base, displacement, scale, 4, index.defaultSIBSegment)

  def quadWordSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1) =
    FixedSizeSIBMemoryLocation(index, base, displacement, scale, 8, index.defaultSIBSegment)
    
  object withSegmentOverride {
    def apply(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1, segment: SegmentRegister) =  
      new SIBMemoryLocation(index, base, displacement, scale, segment)
    
    def byteSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1, segment: SegmentRegister) =
      FixedSizeSIBMemoryLocation(index, base, displacement, scale, 1, segment)
  
    def wordSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1, segment: SegmentRegister) =
      FixedSizeSIBMemoryLocation(index, base, displacement, scale, 2, segment)
    
    def doubleWordSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1, segment: SegmentRegister) =
      FixedSizeSIBMemoryLocation(index, base, displacement, scale, 4, segment)
  
    def quadWordSize(index: SIBIndexRegister, base: SIBBaseRegister, displacement: List[Byte] = List.empty[Byte], scale: Int = 1, segment: SegmentRegister) =
      FixedSizeSIBMemoryLocation(index, base, displacement, scale, 8, segment)

  }
}