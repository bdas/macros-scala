package scala.macros

//http://meta.plasm.us/posts/2013/07/12/vampire-methods-for-structural-types/

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.Context

class body(tree: Any) extends StaticAnnotation

object StructuralMacros {
  def makeInstance = macro makeInstance_impl

  def makeInstance_impl(c: Context) = c.universe.reify[Any] {
    class Workaround {
      def z: Int = 13
      @body(42) def v: Int = macro StructuralMacros.selectField_impl
    }
    new Workaround {}
  }

  def selectField_impl(c: Context) = c.Expr(
    c.macroApplication.symbol.annotations.filter(
      _.tpe <:< c.typeOf[body]
    ).head.scalaArgs.head
  )
}

