package net.virtualvoid.scala

import scala.language.experimental.macros
import scala.reflect.macros.Context

object CInterpolator {
  implicit class CCtx(val ctx: StringContext) extends AnyVal {
    def c(args: Any*): String = macro MacroImpl.body
  }

  private[this] object MacroImpl {
    def body(c: Context { type PrefixType = CCtx })(args: c.Expr[Any]*): c.Expr[String] = {
      import c._
      import c.universe._

      val Apply(_, List(Apply(_, stringExprs))) = c.prefix.tree
      val parts = (stringExprs: List[Tree]) map {
        case Literal(Constant(s: String)) => c.literal(StringContext.treatEscapes(s))
      }

      val pi = parts.iterator
      val ai = args.iterator

      val first = pi.next()
      val bldr = c.Expr[java.lang.StringBuilder](Ident(newTermName("bldr")))

      val innerBlock = c.Expr[Unit] {
        val stats =
          ai.flatMap { a =>
            Seq(
              reify(bldr.splice append a.splice),
              reify(bldr.splice append pi.next().splice)
            )
          }.toList
        Block(stats.map(_.tree), Literal(Constant(())))
      }

      reify {
        val bldr = new java.lang.StringBuilder(first.splice)
        innerBlock.splice
        bldr.toString
      }
    }
  }
}
