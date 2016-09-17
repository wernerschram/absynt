package assembler.memory

import assembler.Condition
import assembler.Encodable

class MemoryPage(val content: Seq[Encodable]) {
  def encodableLocation(encodable: Encodable) = content.indexOf(encodable)

  def getEncodableByCondition(condition: Condition) =
    condition.filter(content).head
  
  def intermediateEncodables(from: Int, to: Int) = 
    if (from < to) {
      content.slice(from + 1, to)
    } else {
      content.slice(to, from)
    }
    
  def encodeByte() = content.flatMap { x => x.encodeByte()(this) }
}