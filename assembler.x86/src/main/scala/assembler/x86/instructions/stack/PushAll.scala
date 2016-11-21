package assembler.x86.instructions.stack

import assembler.x86.ProcessorMode
import assembler.x86.operations.Static

object PushAll {
  implicit val opcode = "pusha"

  private def Static()(implicit processorMode: ProcessorMode) = new Static(0x60.toByte :: Nil, opcode) {
    override def validate = {
      super.validate
      assume(processorMode != ProcessorMode.Long)
    }
  }


//  private object Static extends Static(0x60.toByte :: Nil) {
//    override def validate()(implicit processorMode: ProcessorMode): Boolean = processorMode != ProcessorMode.Long
//  }

  def apply()(implicit processorMode: ProcessorMode) = Static()
}