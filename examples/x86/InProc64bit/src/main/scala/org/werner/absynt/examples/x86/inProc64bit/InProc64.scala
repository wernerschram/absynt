package org.werner.absynt.examples.x86.inProc64bit

import org.werner.absynt.Label
import org.werner.absynt.inproc.InProcSections
import org.werner.absynt.sections.Section

object InProc64 extends App {

  import org.werner.absynt.x86.ProcessorMode.Long._

  val entryLabel: Label = Label.unique

  val text: Section = Section.text(
    Move(RDI, RAX).label(entryLabel) ::
    Add(RSI, RAX) ::
    Return() ::
    Nil
  )

  val inProc = new InProcSections(text :: Nil)

  val addFunc = inProc.functionForLabel[Int, Int, Int](entryLabel)
  val result = addFunc(2,3)
  println(s"2 + 3 = $result")

}
