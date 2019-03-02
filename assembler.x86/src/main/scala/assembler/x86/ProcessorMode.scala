package assembler.x86

import assembler.x86.instructions._
import assembler.x86.operands._
import assembler.x86.operands.memoryaccess._

sealed abstract class ProcessorMode
  extends ImmediateValue.I8086Implicits
  with MemoryAddress.I8086Implicits
  with RegisterMemoryLocation.I8086Implicits
  with FarPointer.I8086Implicits
  with Register.I8086Registers
{
  type LongPointerSize <: ExtendedSize

  def pointer(location: Long): ImmediateValue with WideSize
  def shortPointer(location: Byte): NearPointer with ByteSize = ShortPointer(location)
  def longPointer(location: Int): NearPointer with LongPointerSize
}

object ProcessorMode {

  object Legacy extends ProcessorMode
    with Move.LegacyOperations
    with BasicInteraction.LegacyOperations
    with Interrupt.Operations
    with IO.LegacyOperations
    with Jump.LegacyOperations
    with Stack.LegacyOperations
  {
    override type LongPointerSize = WordSize

    override def pointer(location: Long): ImmediateValue with WideSize = wordImmediate(location.toShort)
    override def longPointer(location: Int): NearPointer with WordSize = LongPointer.realMode(location)
    implicit val processorMode: ProcessorMode = this
  }

  object Real extends ProcessorMode
    with ImmediateValue.I386Implicits
    with MemoryAddress.I386Implicits
    with RegisterMemoryLocation.I386Implicits
    with SIBMemoryLocation.I386Implicits
    with FarPointer.I386Implicits
    with Register.I386Registers
    with Move.RealOperations
    with BasicInteraction.RealOperations
    with Interrupt.Operations
    with IO.RealOperations
    with Jump.RealOperations
    with Stack.RealOperations
  {
    override type LongPointerSize = WordSize

    override def pointer(location: Long): ImmediateValue with WideSize = wordImmediate(location.toShort)
    override def longPointer(location: Int): NearPointer with WordSize = LongPointer.realMode(location)
    implicit val processorMode: ProcessorMode = this
  }

  object Protected extends ProcessorMode
    with ImmediateValue.I386Implicits
    with MemoryAddress.I386Implicits
    with RegisterMemoryLocation.I386Implicits
    with SIBMemoryLocation.I386Implicits
    with FarPointer.I386Implicits
    with Register.I386Registers
    with Move.ProtectedOperations
    with BasicInteraction.ProtectedOperations
    with Interrupt.Operations
    with IO.ProtectedOperations
    with Jump.ProtectedOperations
    with Stack.ProtectedOperations
  {
    override type LongPointerSize = DoubleWordSize

    override def pointer(location: Long): ImmediateValue with WideSize = doubleWordImmediate(location.toInt)
    override def longPointer(location: Int): NearPointer with DoubleWordSize = LongPointer.protectedMode(location)
    implicit val processorMode: ProcessorMode = this
  }

  object Long extends ProcessorMode
    with ImmediateValue.I386Implicits
    with ImmediateValue.X64Implicits
    with MemoryAddress.I386Implicits
    with MemoryAddress.X64Implicits
    with RegisterMemoryLocation.I386Implicits
    with RegisterMemoryLocation.X64Implicits
    with SIBMemoryLocation.I386Implicits
    with SIBMemoryLocation.X64Implicits
    with FarPointer.I386Implicits
    with Register.I386Registers
    with Register.X64Registers
    with Move.LongOperations
    with BasicInteraction.LongOperations
    with Interrupt.Operations
    with IO.LongOperations
    with Jump.LongOperations
    with Stack.LongOperations
  {
    override type LongPointerSize = DoubleWordSize

    override def pointer(location: Long): ImmediateValue with WideSize = quadWordImmediate(location)
    def longPointer(location: Int): NearPointer with DoubleWordSize = LongPointer.protectedMode(location)
    implicit val processorMode: ProcessorMode = this
  }
}
