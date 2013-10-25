package scala.macros

//http://meta.plasm.us/posts/2013/06/21/macro-methods-and-subtypes/

import scala.language.experimental.macros
import shapeless._

trait
HListable

object HListable {
  import scala.reflect.macros.Context

  implicit class HListThisThing[A <: HListable](val a: A) extends AnyVal {
    def members: HList = macro HListable.members_impl[A]
  }

  def members_impl[A <: HListable: c.WeakTypeTag](c: Context): c.Expr[HList] = {
    import c.universe._

    weakTypeOf[A].declarations.collect {
      case m: MethodSymbol if m.isCaseAccessor => m
    }.foldRight(reify(HNil: HList)) {
      case (m, acc) =>
        val value = c.Expr(
          Select(
            Select(c.resetAllAttrs(c.prefix.tree), newTermName("a")),
            m.name
          )
        )
        reify(value.splice :: acc.splice)
    }
  }
}


trait Model

object Model {
  implicit class Mappable[M <: Model](val model: M) extends AnyVal {
    def asMap: Map[String, Any] = macro Macros.asMap_impl[M]
  }

  private object Macros {
    import scala.reflect.macros.Context

    def asMap_impl[T: c.WeakTypeTag](c: Context) = {
      import c.universe._

      val mapApply = Select(reify(Map).tree, newTermName("apply"))
      val model = Select(c.prefix.tree, newTermName("model"))

      val pairs = weakTypeOf[T].declarations.collect {
        case m: MethodSymbol if m.isCaseAccessor =>
          val name = c.literal(m.name.decoded)
          val value = c.Expr(Select(model, m.name))
          reify(name.splice -> value.splice).tree
      }

      c.Expr[Map[String, Any]](Apply(mapApply, pairs.toList))
    }
  }
}