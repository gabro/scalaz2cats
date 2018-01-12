package fix

import scalafix._
import scalafix.syntax._
import scalafix.util.SymbolMatcher
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
      case t @ Term.Select(_, some(_) | none(_))
        if !ctx.tree.contains(scalazOptionSyntaxImport) && !ctx.tree.contains(scalazOptionImport) => ()
    }.length > 0) ctx.addGlobalImport(catsOptionSyntaxImport) else Patch.empty)
  }
}

case class MigrateValidationNel(index: SemanticdbIndex) extends SemanticRule(index, "MigrateValidationNel") {
  private lazy val NonEmptyListScalaz = SymbolMatcher.normalized(
    Symbol("_root_.scalaz.NonEmptyList.")
  )
  private lazy val successNel = SymbolMatcher.normalized(
    Symbol("_root_.scalaz.syntax.ValidationOps.successNel.")
  )
  private lazy val failureNel = SymbolMatcher.normalized(
    Symbol("_root_.scalaz.syntax.ValidationOps.failureNel.")
  )

  private lazy val scalazValidationSyntaxImport = importer"scalaz.syntax.validation._"
  private lazy val catsValidatedSyntaxImport = importer"cats.syntax.validated._"

  // blame scalaz/core/src/main/scala/scalaz/syntax/ApplicativeBuilder.scala for this
  private[this] val cartesianBuilders = SymbolMatcher.normalized((
    "_root_.scalaz.syntax.ApplyOps.`|@|`." ::
    "_root_.scalaz.syntax.ApplicativeBuilder.`|@|`." ::
      (3 to 12).map { i: Int =>
        val post = (3 to i).map { j => s"ApplicativeBuilder$j" }.mkString(".")
        s"_root_.scalaz.syntax.ApplicativeBuilder.$post.`|@|`."
    }.toList).map(Symbol.apply): _*)

  private[this] def replaceOpWithComma(ctx: RuleCtx, op: Term.Name): Patch =
    // replace |@| with ,
    ctx.replaceTree(op, ",") ++
      // remove the space before |@|
      ctx.tokenList
        .leading(op.tokens.head)
        .takeWhile(_.is[Whitespace])
        .map(ctx.removeToken)

  override def fix(ctx: RuleCtx): Patch = {
    ctx.replaceSymbols(
      "scalaz.ValidationNel" -> "cats.data.ValidatedNel",
      "scalaz.Success" -> "cats.data.Validated.Valid",
      "scalaz.Failure" -> "cats.data.Validated.Invalid",
      "scalaz.syntax.ValidationOps.successNel" -> "validNel",
      "scalaz.syntax.ValidationOps.failureNel" -> "invalidNel"
    ) + ctx.tree.collect {
      case t @ importer"scalaz.syntax.validation._" =>
        ctx.replaceTree(t, importer"cats.syntax.validated._".syntax)
      case t @ importer"scalaz.syntax.apply._" =>
        ctx.replaceTree(t, importer"cats.syntax.apply._".syntax)
      case t @ importer"scalaz.{..$ips}" =>
        ips.collect {
          case NonEmptyListScalaz(i: Importee) =>
            ctx.removeImportee(i) + ctx.addGlobalImport(importer"cats.data.NonEmptyList")
        }.asPatch
      case Term.ApplyInfix(_, cartesianBuilders(op: Term.Name), _, _) =>
        replaceOpWithComma(ctx, op)
      case Term.Apply(t @ Term.ApplyInfix(_, cartesianBuilders(_), _, _), _) =>
        ctx.addRight(t, ".mapN")
      case Term.Apply(NonEmptyListScalaz(t), _) =>
        val x = ctx.tokenList.next(t.tokens.last)
        ctx.addRight(t, ".of")
    }.asPatch + (if (ctx.tree.collect {
      case t @ Term.Select(_, successNel(_) | failureNel(_))
        if !ctx.tree.contains(scalazValidationSyntaxImport) => ()
    }.length > 0) ctx.addGlobalImport(catsValidatedSyntaxImport) else Patch.empty)
  }
}

case class MigrateValidation(index: SemanticdbIndex) extends SemanticRule(index, "MigrateValidation") {
  override def fix(ctx: RuleCtx): Patch = {
    ctx.replaceSymbols(
      "scalaz.Validation" -> "cats.data.Validated",
      "scalaz.Success" -> "cats.data.Validated.Valid",
      "scalaz.Failure" -> "cats.data.Validated.Invalid"
    )
  }
}

case class RemoveGlobalImports(index: SemanticdbIndex) extends SemanticRule(index, "RemoveGlobalImports") {
  override def fix(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case t @ importer"scalaz._" =>
        ctx.removeImportee(t.asInstanceOf[Importer].importees.head)
      case t @ importer"Scalaz._" =>
        ctx.removeImportee(t.asInstanceOf[Importer].importees.head)
      case t @ importer"scalaz.Scalaz._" =>
        ctx.removeImportee(t.asInstanceOf[Importer].importees.head)
    }.asPatch
  }
}
