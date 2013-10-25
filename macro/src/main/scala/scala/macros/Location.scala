package scala.macros

import scala.reflect.macros.Context
import scala.language.experimental.macros

case class Location(line:Int,column:Int,sourcePath:String)

object LocationMacro {

  def currentLocation:Location = macro impl

  def impl(c:Context):c.Expr[Location] ={
    import c.universe._
    val pos = c.macroApplication.pos

    reify[Location](Location(c.literal(pos.line).splice,c.literal(pos.column).splice,c.literal(pos.source.path).splice))
  }
}
