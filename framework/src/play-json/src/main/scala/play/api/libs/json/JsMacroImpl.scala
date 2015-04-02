/*
 * Copyright (C) 2009-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package play.api.libs.json

import scala.language.higherKinds
import scala.reflect.macros.whitebox.Context
import language.experimental.macros

object JsMacroImpl {

  def formatImpl[A: c.WeakTypeTag](c: Context): c.Tree = {
    ???
  }

  def readsImpl[A: c.WeakTypeTag](c: Context): c.Tree = {
    ???
  }

  def writesImpl[A: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    val T = weakTypeOf[A]
    val symbol = T.typeSymbol
    if (!symbol.asClass.isCaseClass)
      c.abort(c.enclosingPosition, s"$symbol is not a case class")
    if (!symbol.isStatic)
      c.abort(c.enclosingPosition, s"$symbol is not static")
    def fields(tpe: Type) = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
    }.get.paramLists.head.map { field ⇒
      val name = field.name.toTermName
      val typeSign = tpe.decl(name).typeSignature
      name → typeSign
    }
    def select(symbol: Symbol) = q"$symbol"
    val arguments: Seq[Tree] = fields(T).map {
      case (name, typeSign) ⇒
        q"""
        ($name,
         implicitly[Writes[$typeSign]].apply(value.$name))
       """
    }
    val reflect = q"""Apply(Select(Ident(play.api.libs.json.JsObject), TermName("apply")), List(..$arguments))"""
    val implicitName = TermName(symbol.name.encodedName.toString ++ "Writes")
    q"""
      implicit object $implicitName extends Writes[$T] {
        override def writes(value: $T): JsValue = $reflect
      }
      $implicitName
    """
  }
}
