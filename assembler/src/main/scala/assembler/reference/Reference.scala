package assembler.reference

import assembler.{Label, Resource}

trait Reference extends Resource {
  def target: Label

}
