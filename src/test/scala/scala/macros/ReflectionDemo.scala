package scala.macros

trait Event

case class SomeEvent() extends Event

case class ReflectionDemo( x: Int, y: String) {

  def applyEvent(event: SomeEvent): ReflectionDemo = {
    copy()
  }

}

import scala.reflect.runtime.universe._

object Util extends App {

  def isApplyEvent(symbol: MethodSymbol, value: Type) = {
    symbol.name.decoded == "applyEvent"
  }

  def find() = {

    val methods = typeOf[ReflectionDemo].members.collect {
      case m: MethodSymbol if isApplyEvent(m, typeOf[Event]) =>  m
    }

    println(methods)

  }

  find()
}