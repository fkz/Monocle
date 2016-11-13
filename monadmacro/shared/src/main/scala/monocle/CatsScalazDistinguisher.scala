package monocle

object MonadLibraryUse {
  val value = MonadLibrary.UseCats
}

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox

class OnlyInCats extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro CatsScalazDistinguisher.onlyInCats
}

class OnlyInScalaz extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro CatsScalazDistinguisher.onlyInScalaz
}

sealed trait MonadLibrary

object MonadLibrary {
  object UseCats extends MonadLibrary
  object UseScalaz extends MonadLibrary
}

class CatsScalazDistinguisher(val c: blackbox.Context) {
  import c.universe._

  def onlyIn(annottees: Seq[c.Expr[Any]], lib: MonadLibrary): c.Expr[Any] = {
    val t = c.typecheck(q"monocle.MonadLibraryUse.value", c.TYPEmode, silent = true)
    if (t.tpe =:= typeOf[MonadLibrary.UseCats.type]) {
      lib match {
        case MonadLibrary.UseCats =>
          c.Expr[Any](annottees.map(_.tree).head)
        case MonadLibrary.UseScalaz => c.Expr[Any](q"")
      }
    }
    else if (t.tpe =:= typeOf[MonadLibrary.UseScalaz.type]) {
      lib match {
        case MonadLibrary.UseScalaz =>
          c.Expr[Any](annottees.map(_.tree).head)
        case MonadLibrary.UseCats => c.Expr[Any](q"")
      }
    }
    else
      c.abort(c.enclosingPosition, "this macro needs access to monocle.typelevelLibrary, which has to be either UseCats or UseScalaz")
  }

  def onlyInScalaz(annottees: c.Expr[Any]*): c.Expr[Any] = onlyIn(annottees, MonadLibrary.UseScalaz)
  def onlyInCats(annottees: c.Expr[Any]*): c.Expr[Any] = onlyIn(annottees, MonadLibrary.UseCats)

}
