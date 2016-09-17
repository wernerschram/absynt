package assembler.memory

import assembler.Condition
import assembler.Encodable

class MemoryPage(val content: List[Encodable]) {
  def encodableLocation(encodable: Encodable) = new PageLocation(content.indexOf(encodable))

  def getInstructionByCondition(condition: Condition) =
    condition.filterList(content).head
  
  def slice(from: PageLocation, to: PageLocation) = 
    if (from < to) {
      content.slice(from.value + 1, to.value)
    } else {
      content.slice(to.value, from.value)
    }
    
  def encodeByte() = content.flatMap { x => x.encodeByte()(this) }
}

class PageLocation(val value: Int) extends Comparable[PageLocation] {
  def compareTo(other: PageLocation) = {
    this.value - other.value
  }
  def >(that: PageLocation) = this.value > that.value
  def <(that: PageLocation) = this.value < that.value
}