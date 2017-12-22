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

  private lazy val scalazEitherSyntaxImport = importer"scalaz.syntax.either._"
  private lazy val catsEitherSyntaxImport = importer"cats.syntax.either._"

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
    }.asPatch + ctx.replaceSymbols(
      "scalaz.syntax.EitherOps.left" -> "asLeft",
      "scalaz.syntax.EitherOps.right" -> "asRight",
      "scalaz.`\\/-`" -> "scala.Right",
      "scalaz.`-\\/`" -> "scala.Left"
    ) + (if (ctx.tree.collect {
      case t @ Term.Select(_, left(_) | right(_)) if !ctx.tree.contains(scalazEitherSyntaxImport) => ()
    }.length > 0) ctx.addGlobalImport(catsEitherSyntaxImport) else Patch.empty)
  }
}

case class MigrateOptionSyntax(index: SemanticdbIndex) extends SemanticRule(index, "MigrateOptionSyntax") {
  private lazy val scalazOptionSyntaxImport = importer"scalaz.syntax.std.option._"
  private lazy val scalazOptionImport = importer"scalaz.syntax.option._"
  private lazy val catsOptionSyntaxImport = importer"cats.syntax.option._"
  private lazy val some = SymbolMatcher.normalized(
    Symbol("_root_.scalaz.syntax.std.OptionIdOps.some.")
  )
  private lazy val none = SymbolMatcher.normalized(
    Symbol("_root_.scalaz.syntax.std.OptionFunctions.none.")
  )

  override def fix(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case t @ importer"scalaz.syntax.std.option._" =>
        ctx.replaceTree(t, catsOptionSyntaxImport.syntax)
      case t @ importer"scalaz.std.option._" =>
        if (!ctx.tree.contains(scalazOptionSyntaxImport)) ctx.replaceTree(t, catsOptionSyntaxImport.syntax)
        else ctx.removeImportee(t.asInstanceOf[Importer].importees.head)
      case t @ importer"scalaz.std.option._" if !ctx.tree.contains(scalazOptionSyntaxImport) =>
        ctx.replaceTree(t, catsOptionSyntaxImport.syntax)
    }.asPatch + (if (ctx.tree.collect {
      case t @ Term.Select(_, some(_) | none (_))
        if !ctx.tree.contains(scalazOptionSyntaxImport) && !ctx.tree.contains(scalazOptionImport) => ()
    }.length > 0) ctx.addGlobalImport(catsOptionSyntaxImport) else Patch.empty)
  }
}

case class RemoveGlobalImports(index: SemanticdbIndex) extends SemanticRule(index, "RemoveGlobalImports") {
  override def fix(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case t @ importer"scalaz._" =>
        ctx.removeImportee(t.asInstanceOf[Importer].importees.head)
      case t @ importer"Scalaz._" =>
        ctx.removeImportee(t.asInstanceOf[Importer].importees.head)
    }.asPatch
  }
}
