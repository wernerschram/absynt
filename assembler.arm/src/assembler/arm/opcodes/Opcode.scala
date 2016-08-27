package assembler.arm.opcodes

import assembler.ListExtensions._
import assembler.arm.operands.Operand
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.ProcessorMode
import assembler.arm.instructions.ARMInstruction
import assembler.MemoryPage
import java.nio.ByteBuffer
import assembler.arm.operands.Condition

abstract class Opcode(val mnemonic: String) {
}