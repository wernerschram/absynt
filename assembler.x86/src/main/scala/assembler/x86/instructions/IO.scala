package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operations.OperandInfo.OperandOrder._
import assembler.x86.operations._

object IO extends {

  sealed trait I8086Input {
    val mnemonic: String = "in"

    private def Imm8ToAL(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
      new Static(0xE4.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
        override protected def implicitInit(): Unit =
          addOperand(OperandInfo.implicitOperand(Accumulator.LowByte, destination))

        override def immediateOrder: OperandOrder = source

        override val immediate: ImmediateValue with ByteSize = immediateValue
      }

    private def Imm8ToAX(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
      new Static(0xE5.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
        override protected def implicitInit(): Unit =
          addOperand(OperandInfo.implicitOperand(Accumulator.Word, destination))

        override def immediateOrder: OperandOrder = source

        override val immediate: ImmediateValue with ByteSize = immediateValue
      }

    private def DXToAL()(implicit processorMode: ProcessorMode) = new Static(0xEC.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
      override protected def implicitInit(): Unit = {
        addOperand(OperandInfo.implicitPort(Data.Word, source))
        addOperand(OperandInfo.implicitOperand(Accumulator.LowByte, destination))
      }
    }

    private def DXToAX()(implicit processorMode: ProcessorMode) = new Static(0xED.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
      override protected def implicitInit(): Unit = {
        addOperand(OperandInfo.implicitPort(Data.Word, source))
        addOperand(OperandInfo.implicitOperand(Accumulator.Word, destination))
      }
    }

    def apply(immediate: ImmediateValue with ByteSize, destination: Accumulator.LowByte.type)(implicit processorMode: ProcessorMode): Static =
      Imm8ToAL(immediate)

    def apply(immediate: ImmediateValue with ByteSize, destination: Accumulator.Word.type)(implicit processorMode: ProcessorMode): Static =
      Imm8ToAX(immediate)

    def apply(port: Data.Word.type, destination: Accumulator.LowByte.type)(implicit processorMode: ProcessorMode): Static =
      DXToAL

    def apply(port: Data.Word.type, destination: Accumulator.Word.type)(implicit processorMode: ProcessorMode): Static =
      DXToAX
  }

  sealed trait I386Input extends I8086Input {

    private def Imm8ToEAX(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
      new Static(0xE5.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
        override protected def implicitInit(): Unit =
          addOperand(OperandInfo.implicitOperand(Accumulator.DoubleWord, destination))

        override def immediateOrder: OperandOrder = source

        override val immediate: ImmediateValue with ByteSize = immediateValue
      }

    private def DXToEAX()(implicit processorMode: ProcessorMode) = new Static(0xED.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
      override protected def implicitInit(): Unit = {
        addOperand(OperandInfo.implicitPort(Data.Word, source))
        addOperand(OperandInfo.implicitOperand(Accumulator.DoubleWord, destination))
      }
    }

    def apply(immediate: ImmediateValue with ByteSize, destination: Accumulator.DoubleWord.type)(implicit processorMode: ProcessorMode): Static =
      Imm8ToEAX(immediate)

    def apply(port: Data.Word.type, destination: Accumulator.DoubleWord.type)(implicit processorMode: ProcessorMode): Static =
      DXToEAX
  }

  sealed trait I8086Output {

    val mnemonic: String = "out"

    private def ALToImm8(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
      new Static(0xE6.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
        override protected def implicitInit(): Unit =
          addOperand(OperandInfo.implicitOperand(Accumulator.LowByte, source))

        override def immediateOrder: OperandOrder = destination

        override val immediate: ImmediateValue with ByteSize = immediateValue
      }

    private def AXToImm8(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
      new Static(0xE7.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
        override protected def implicitInit(): Unit =
          addOperand(OperandInfo.implicitOperand(Accumulator.Word, source))

        override def immediateOrder: OperandOrder = destination

        override val immediate: ImmediateValue with ByteSize = immediateValue
      }

    private def ALToDX()(implicit processorMode: ProcessorMode) =
      new Static(0xEE.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        override protected def implicitInit(): Unit = {
          addOperand(OperandInfo.implicitPort(Data.Word, destination))
          addOperand(OperandInfo.implicitOperand(Accumulator.LowByte, source))
        }
      }

    private def AXToDX()(implicit processorMode: ProcessorMode) =
      new Static(0xEF.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate  {
        override protected def implicitInit(): Unit = {
          addOperand(OperandInfo.implicitPort(Data.Word, destination))
          addOperand(OperandInfo.implicitOperand(Accumulator.Word, source))
        }
      }

    def apply(destination: Accumulator.LowByte.type, immediate: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode): Static with Immediate[ByteSize] =
      ALToImm8(immediate)

    def apply(destination: Accumulator.Word.type, immediate: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode): Static with Immediate[ByteSize] =
      AXToImm8(immediate)


    def apply(destination: Accumulator.LowByte.type, port: Data.Word.type)(implicit processorMode: ProcessorMode): Static =
      ALToDX

    def apply(destination: Accumulator.Word.type, port: Data.Word.type)(implicit processorMode: ProcessorMode): Static =
      AXToDX
  }

  sealed trait I386Output extends I8086Output {
    private def EAXToImm8(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
      new Static(0xE7.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
        override protected def implicitInit(): Unit =
          addOperand(OperandInfo.implicitOperand(Accumulator.DoubleWord, source))

        override def immediateOrder: OperandOrder = destination

        override val immediate: ImmediateValue with ByteSize = immediateValue
      }


    private def EAXToDX()(implicit processorMode: ProcessorMode) =
      new Static(0xEF.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate  {
        override protected def implicitInit(): Unit = {
          addOperand(OperandInfo.implicitPort(Data.Word, destination))
          addOperand(OperandInfo.implicitOperand(Accumulator.DoubleWord, source))
        }
      }

    def apply(destination: Accumulator.DoubleWord.type, port: Data.Word.type)(implicit processorMode: ProcessorMode): Static =
      EAXToDX

    def apply(destination: Accumulator.DoubleWord.type, immediate: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode): Static with Immediate[ByteSize] =
      EAXToImm8(immediate)
  }

  trait LegacyOperations {
    object Input extends I8086Input
    object Output extends I8086Output
  }

  trait RealOperations {
    object Input extends I386Input
    object Output extends I386Output
  }

  trait ProtectedOperations {
    object Input extends I386Input
    object Output extends I386Output
  }

  trait LongOperations {
    object Input extends I386Input
    object Output extends I386Output
  }

}
