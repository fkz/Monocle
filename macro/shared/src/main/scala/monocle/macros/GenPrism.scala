package monocle.macros

import monocle.Prism

import scala.reflect.macros.blackbox

object GenPrism {
  /** generate a [[Prism]] between `S` and a subtype `A` of `S` */
  def apply[S, A <: S]: Prism[S, A] = macro GenPrismImpl.genPrism_impl[S, A]
}

@macrocompat.bundle
private class GenPrismImpl(val c: blackbox.Context) {
  def genPrism_impl[S: c.WeakTypeTag, A: c.WeakTypeTag]: c.Expr[Prism[S, A]] = {
    import c.universe._

    val (sTpe, aTpe) = (weakTypeOf[S], weakTypeOf[A])

    val sTpeSym = sTpe.typeSymbol.companion
    c.Expr[Prism[S, A]](q"""
      import monocle.Prism
      import monocle.catssupport.Implicits.\/
                           |
      new Prism[$sTpe, $aTpe]{
        def getOrModify(s: $sTpe): $sTpe \/ $aTpe =
          if(s.isInstanceOf[$aTpe]) \/.right(s.asInstanceOf[$aTpe])
          else \/.left(s)

        def reverseGet(a: $aTpe): $sTpe =
          a.asInstanceOf[$sTpe]

        def getOption(s: $sTpe): Option[$aTpe] =
          if(s.isInstanceOf[$aTpe]) Some(s.asInstanceOf[$aTpe])
          else None
      }
    """)
  }
}
