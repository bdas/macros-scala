package scala.macros

import scala.reflect.runtime.universe._

object ASTPrinters extends App{

  val expr = reify{ final class C { def x = 2 } }
  println(expr)

  val tree = expr.tree
  println(tree)

  println(showRaw(expr))
  println(show(tree))

  println(showRaw(reify{Location(1,2,"")}))

}
