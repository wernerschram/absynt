package org.werner.absynt.inproc

import com.sun.jna._
import org.werner.absynt.inproc.InProcSections.ReturnType
import org.werner.absynt.resource.EncodableConversion._
import org.werner.absynt.resource.{AlignmentFiller, Encodable, Labeled, Resource}
import org.werner.absynt.sections.Section
import org.werner.absynt.{Application, Label}
import sun.misc.Unsafe

class InProcSections(val sections: Seq[Section]) extends Application {

  lazy val unsafe: Unsafe = {
    val theUnsafe = classOf[sun.misc.Unsafe].getDeclaredField("theUnsafe")
    theUnsafe.setAccessible(true)
    theUnsafe.get(null).asInstanceOf[sun.misc.Unsafe]
  }

  private val pageSize = unsafe.pageSize

  def content: Seq[Resource] = sections.flatMap(s => alignmentFillers(s) +: s.content)

  lazy val encodableContent: Seq[Encodable] = content.encodables(encodablesForDependencies(content.dependentResources))

  lazy val encodeBytes: Seq[Byte] =
    encodableContent.encodeByte

  private val contentLength = encodeBytes.size

  // We need contentLenght bytes, but we want to start on the beginning of a page and end on a completely allocated page,
  // so we can set the page protection properties.
  private val allocationSize = contentLength + 2 * pageSize - contentLength % pageSize

  private val allocationAddress = unsafe.allocateMemory(allocationSize)

  private val startAddress = allocationAddress + pageSize - allocationAddress  % pageSize

  encodeBytes.zipWithIndex.foreach{
    case (value, i) => unsafe.putByte(startAddress + i, value)
  }

  Libc.mprotect(new Pointer(startAddress), contentLength + pageSize - contentLength % pageSize, Libc.Protection.Read | Libc.Protection.Exec)

  val labelPointers: Map[Label, Pointer] = encodableContent.foldLeft((startAddress, Map.empty[Label, Pointer])){
    case ((lastOffset, labels), encodable: Encodable with Labeled) => (lastOffset + encodable.size, labels + (encodable.label -> new Pointer(lastOffset)))
    case ((lastOffset, labels), encodable: Encodable) => (lastOffset + encodable.size, labels)
  }._2


  def functionForLabel[Out: ReturnType](label: Label): () => Out = {
    val function = Function.getFunction(labelPointers(label))
    () => implicitly[ReturnType[Out]].invoke(function, Array.empty)
  }

  def functionForLabel[Out: ReturnType, In1](label: Label): In1 => Out = {
    val function = Function.getFunction(labelPointers(label))
    arg1: In1 => implicitly[ReturnType[Out]].invoke(function, Array(arg1.asInstanceOf[AnyRef]))
  }

  def functionForLabel[Out: ReturnType, In1, In2](label: Label): (In1, In2) => Out = {
    val function = Function.getFunction(labelPointers(label))
    (arg1: In1, arg2: In2) => implicitly[ReturnType[Out]].invoke(function, Array(arg1.asInstanceOf[AnyRef], arg2.asInstanceOf[AnyRef]))
  }

  override lazy val alignmentFillers: Map[Section, AlignmentFiller] = sections.map(s => s -> AlignmentFiller(s)).toMap

  override def startOffset: Int = startAddress.toInt
}

object InProcSections {
   sealed abstract class ReturnType[Out] {
    def invoke(function: Function, params: Array[AnyRef]): Out
  }

  implicit final def intReturnType: ReturnType[Int] = new ReturnType[Int] {
    override def invoke(function: Function, params: Array[AnyRef]): Int = function.invokeInt(params)
  }

}

object Libc {

  trait Libc extends Library {
    def mprotect(addr: Pointer, len: Long, prot: Int): Int
  }

  private val lib = Native.load("c", classOf[Libc])

  sealed case class Protection(code: Int) {
    def |(p: Protection) =
      Protection(this.code | p.code)
  }

  object Protection {
    val None = Protection(0)
    val Read = Protection(1)
    val Write = Protection(2)
    val Exec = Protection(4)
  }

  def mprotect(addr: Pointer, len: Long, prot: Protection): Int = lib.mprotect(addr, len, prot.code)
}
