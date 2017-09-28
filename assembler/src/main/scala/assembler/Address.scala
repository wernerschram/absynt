package assembler

trait Address {
  def add(offset: Offset): Address

  def +  (that: Offset): Address = this add that
}

trait Offset
