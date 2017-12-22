package fix

import scalafix._
import scalafix.util._
import scala.meta._

case class MigrateEither(index: SemanticdbIndex) extends SemanticRule(index, "MigrateEither") {

  private lazy val \/ = SymbolMatcher.normalized(
    Symbol("_root_.scalaz.`\\/`.")
  )

  private lazy val \/- = SymbolMatcher.normalized(
    Symbol("_root_.scalaz.`\\/-`.")
  )

  private lazy val -\/ = SymbolMatcher.normalized(
    Symbol("_root_.scalaz.`-\\/`.")
  )

  override def fix(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case t @ Type.ApplyInfix(lhs, \/(_), rhs) =>
        ctx.replaceTree(t, q"Either[$lhs, $rhs]".syntax)
      case t @ Type.Apply(\/(_), args) =>
        ctx.replaceTree(t, q"Either[${args(0)}, ${args(1)}]".syntax)
      case t @ importer"scalaz.syntax.either._" =>
        ctx.replaceTree(t, importer"cats.syntax.either._".syntax)
      case t @ Importee.Name(\/(_) | \/-(_) | -\/(_)) =>
        ctx.removeImportee(t)
    }.asPatch + ctx.replaceSymbols(
      "scalaz.syntax.EitherOps.left" -> "asLeft",
      "scalaz.syntax.EitherOps.right" -> "asRight",
      "scalaz.`\\/-`" -> "Right",
      "scalaz.`-\\/`" -> "Left"
    )
  }
}
