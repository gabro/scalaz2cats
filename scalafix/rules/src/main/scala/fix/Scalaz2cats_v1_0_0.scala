package fix

import scalafix._
import scalafix.util._
import scala.meta._
import scala.meta.contrib._

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

  private lazy val left = SymbolMatcher.normalized(
    Symbol("_root_.scalaz.syntax.EitherOps.left.")
  )

  private lazy val right = SymbolMatcher.normalized(
    Symbol("_root_.scalaz.syntax.EitherOps.right.")
  )

  private val scalazEitherSyntaxImport = importer"scalaz.syntax.either._"
  private val catsEitherSyntaxImport = importer"cats.syntax.either._"

  override def fix(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case t @ Type.ApplyInfix(lhs, \/(_), rhs) =>
        ctx.replaceTree(t, q"Either[$lhs, $rhs]".syntax)
      case t @ Type.Apply(\/(_), args) =>
        ctx.replaceTree(t, q"Either[${args(0)}, ${args(1)}]".syntax)
      case t @ importer"scalaz.syntax.either._" =>
        ctx.replaceTree(t, catsEitherSyntaxImport.syntax)
      case t @ Importee.Name(\/(_) | \/-(_) | -\/(_)) =>
        ctx.removeImportee(t)
      case t @ Term.Select(_, left(_) | right(_)) if !ctx.tree.contains(scalazEitherSyntaxImport) =>
        ctx.addGlobalImport(catsEitherSyntaxImport)
    }.asPatch + ctx.replaceSymbols(
      "scalaz.syntax.EitherOps.left" -> "asLeft",
      "scalaz.syntax.EitherOps.right" -> "asRight",
      "scalaz.`\\/-`" -> "scala.Right",
      "scalaz.`-\\/`" -> "scala.Left"
    )
  }
}
