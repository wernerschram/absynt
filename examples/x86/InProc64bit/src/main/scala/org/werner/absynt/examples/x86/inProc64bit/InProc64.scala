/*
 * Copyright 2019 Werner Schram
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

package org.werner.absynt.examples.x86.inProc64bit

import org.werner.absynt.Label
import org.werner.absynt.inproc.InProcApplication
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

  val inProc = new InProcApplication(text :: Nil)

  val addFunc = inProc.functionForLabel[Int, Int, Int](entryLabel)
  val result = addFunc(2,3)
  println(s"2 + 3 = $result")

  inProc.close()
}
