package fix

import fix.ScalafixUtils._
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
      case t @ Type.Apply(\/(_), List(rtpe, ltpe)) =>
        ctx.replaceTree(t, q"Either[$rtpe, $ltpe]".syntax)
      case t @ importer"scalaz.syntax.either._" =>
        ctx.replaceTree(t, catsEitherSyntaxImport.syntax)
      case t @ Importee.Name(\/(_) | \/-(_) | -\/(_)) =>
        ctx.removeImportee(t)
    }.asPatch + ctx.replaceSymbols(
      "scalaz.syntax.EitherOps.left" -> "asLeft",
      "scalaz.syntax.EitherOps.right" -> "asRight",
      "scalaz.`\\/`.`|`" -> "getOrElse",
      "scalaz.`\\/-`" -> "scala.Right",
      "scalaz.`-\\/`" -> "scala.Left"
    ) + addImportsIfNeeded(ctx) {
      case Term.Select(_, left(_) | right(_)) if !ctx.tree.contains(scalazEitherSyntaxImport) =>
        catsEitherSyntaxImport
    }
  }
}

case class MigrateOptionSyntax(index: SemanticdbIndex)
    extends SemanticRule(index, "MigrateOptionSyntax") {
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
        if (!ctx.tree.contains(scalazOptionSyntaxImport))
          ctx.replaceTree(t, catsOptionSyntaxImport.syntax)
        else ctx.removeImportee(t.asInstanceOf[Importer].importees.head)
      case t @ importer"scalaz.std.option._" if !ctx.tree.contains(scalazOptionSyntaxImport) =>
        ctx.replaceTree(t, catsOptionSyntaxImport.syntax)
    }.asPatch + ctx.replaceSymbols(
      "scalaz.syntax.std.OptionOps.`|`" -> "getOrElse"
    ) + addImportsIfNeeded(ctx) {
      case Term.Select(_, some(_) | none(_))
          if !ctx.tree.contains(scalazOptionSyntaxImport)
            && !ctx.tree.contains(scalazOptionImport) =>
        catsOptionSyntaxImport
    }
  }
}

case class MigrateValidationNel(index: SemanticdbIndex)
    extends SemanticRule(index, "MigrateValidationNel") {
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
  private[this] val cartesianBuilders = SymbolMatcher.normalized(
    ("_root_.scalaz.syntax.ApplyOps.`|@|`." ::
      "_root_.scalaz.syntax.ApplicativeBuilder.`|@|`." ::
      (3 to 12).map { i: Int =>
      val post = (3 to i)
        .map { j =>
          s"ApplicativeBuilder$j"
        }
        .mkString(".")
      s"_root_.scalaz.syntax.ApplicativeBuilder.$post.`|@|`."
    }.toList).map(Symbol.apply): _*
  )

  private[this] val cartesianAppliesRenames: Map[String, String] = {
    val applicativeArityBuilders = (3 to 12).map { i: Int =>
      val post = (3 to i)
        .map { j =>
          s"ApplicativeBuilder$j"
        }
        .mkString(".")
      s"_root_.scalaz.syntax.ApplicativeBuilder.$post.apply." -> "mapN"
    }.toList

    (Seq(
      s"_root_.scalaz.syntax.ApplicativeBuilder.apply." -> "mapN"
    ) ++ applicativeArityBuilders).toMap
  }

  private[this] val cartesianOps =
    SymbolMatcher.normalized(cartesianAppliesRenames.keys.map(Symbol.apply).toSeq: _*)

  private def addValidatedSyntaxImportIfNeeded(ctx: RuleCtx): Patch =
    addImportsIfNeeded(ctx) {
      case Term.Select(_, successNel(_) | failureNel(_))
          if !ctx.tree.contains(scalazValidationSyntaxImport) =>
        catsValidatedSyntaxImport
    }

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
      case importer"scalaz.{..$ips}" =>
        ips.collect {
          case NonEmptyListScalaz(i: Importee) =>
            ctx.removeImportee(i) + ctx.addGlobalImport(importer"cats.data.NonEmptyList")
        }.asPatch
      case Term.ApplyInfix(_, cartesianBuilders(op: Term.Name), _, _) =>
        replaceOpWithComma(ctx, op)
      case Term.ApplyInfix(lhs, cartesianOps(_), _, _) =>
        wrapInParensIfNeeded(ctx, lhs)
      case Term.Apply(t @ Term.ApplyInfix(_, cartesianBuilders(_), _, _), _) =>
        ctx.addRight(t, ".mapN")
      case Term.Apply(NonEmptyListScalaz(t), _) =>
        ctx.addRight(t, ".of")
    }.asPatch +
      ctx.replaceSymbols(cartesianAppliesRenames.toSeq: _*) +
      addValidatedSyntaxImportIfNeeded(ctx)
  }

}

case class MigrateValidation(index: SemanticdbIndex)
    extends SemanticRule(index, "MigrateValidation") {
  override def fix(ctx: RuleCtx): Patch = {
    ctx.replaceSymbols(
      "scalaz.Validation" -> "cats.data.Validated",
      "scalaz.Success" -> "cats.data.Validated.Valid",
      "scalaz.Failure" -> "cats.data.Validated.Invalid"
    )
  }
}

case class RemoveGlobalImports(index: SemanticdbIndex)
    extends SemanticRule(index, "RemoveGlobalImports") {
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

case class MigrateEqual(index: SemanticdbIndex) extends SemanticRule(index, "MigrateEqual") {

  private lazy val scalazEqualSyntaxImport = importer"scalaz.syntax.equal._"
  private lazy val scalazEqualImport = importer"scalaz.Equal"
  private lazy val catsEqualSyntaxImport = importer"cats.syntax.eq._"
  private lazy val catsEqualImport = importer"cats.Eq"

  private[this] val Equal = SymbolMatcher.normalized(
    Symbol("_root_.scalaz.Equal.")
  )

  private[this] val catsEqual = SymbolMatcher.normalized(
    Symbol("_root_.cats.Eq.")
  )

  private[this] val `===` = SymbolMatcher.normalized(
    Symbol("_root_.scalaz.syntax.EqualOps.`===`.")
  )
  private[this] val `=/=` = SymbolMatcher.normalized(
    Symbol("_root_.scalaz.syntax.EqualOps.`=/=`.")
  )

  private[this] val `/==` = SymbolMatcher.normalized(
    Symbol("_root_.scalaz.syntax.EqualOps.`/==`.")
  )

  private[this] val `≠` = SymbolMatcher.normalized(
    Symbol("_root_.scalaz.syntax.EqualOps.`≠`.")
  )

  private[this] val `≟` = SymbolMatcher.normalized(
    Symbol("_root_.scalaz.syntax.EqualOps.`≟`.")
  )

  override def fix(ctx: RuleCtx): Patch = {
    val importChecker = addImportsIfNeeded(ctx) _

    ctx.tree.collect {
      case t @ importer"scalaz.syntax.equal._" =>
        ctx.replaceTree(t, catsEqualSyntaxImport.syntax)
      case t @ importer"scalaz.Equal" =>
        ctx.replaceTree(t, catsEqualImport.syntax)
      case t @ Type.Apply(Equal(_), List(tpe)) =>
        ctx.replaceTree(t, q"Eq[$tpe]".syntax)
      case Term.Select(a @ Equal(_), _) =>
        ctx.replaceTree(a, "Eq")
      case Type.Param(_, _, _, _, _, List(a @ Equal(_))) =>
        ctx.replaceTree(a, "Eq")
    }.asPatch + ctx.replaceSymbols(
      "scalaz.syntax.EqualOps.`=/=`" -> "`=!=`",
      "scalaz.syntax.EqualOps.`/==`" -> "`=!=`",
      "scalaz.syntax.EqualOps.`≠`" -> "`=!=`",
      "scalaz.syntax.EqualOps.`≟`" -> "`===`",
      "scalaz.Equal.equalA" -> "fromUniversalEquals",
      "scalaz.Equal.equalBy" -> "by",
      "scalaz.Equal.equal" -> "eqv"
    ) + importChecker {
      case Term.ApplyInfix(_, `===`(_) | `=/=`(_) | `/==`(_) | `≠`(_) | `≟`(_), _, _)
          if !ctx.tree.contains(scalazEqualSyntaxImport) =>
        catsEqualSyntaxImport
    } + importChecker {
      case Term.Select(Equal(_), _) if !ctx.tree.contains(scalazEqualImport) => catsEqualImport
    }
  }
}

case class MigrateBoolean(index: SemanticdbIndex) extends SemanticRule(index, "MigrateBoolean") {
  private lazy val scalazBooleanSyntaxImport = importer"scalaz.syntax.std.boolean._"
  private lazy val catsBooleanSyntaxImport = importer"mouse.boolean._"
  private[this] val booleanSymbolOr = SymbolMatcher.normalized(
    Symbol.apply("_root_.scalaz.syntax.std.BooleanOps.`?`.")
  )

  private[this] val booleanSymbolOrTwo = SymbolMatcher.normalized(
    Symbol.apply("_root_.scalaz.syntax.std.BooleanOps.Conditional.`|`.")
  )

  private[this] val booleanEither = SymbolMatcher.normalized(
    Symbol.apply("_root_.scalaz.syntax.std.BooleanOps.either.")
  )

  private[this] val booleanEitherOr = SymbolMatcher.normalized(
    Symbol.apply("_root_.scalaz.syntax.std.BooleanOps.ConditionalEither.or.")
  )

  // TODO: also need to support a `true.either(12).or("abc")` case
  override def fix(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case t @ importer"scalaz.syntax.std.boolean._" =>
        ctx.replaceTree(t, catsBooleanSyntaxImport.syntax)
      case Term.ApplyInfix(
          Term.ApplyInfix(_, booleanSymbolOr(op1: Term.Name), _, List(firstArg)),
          booleanSymbolOrTwo(op2: Term.Name),
          _,
          List(secondArg)
          ) =>
        ctx.replaceTree(op1, "fold") +
          replaceOpWithComma(ctx, op2) +
          removeWhiteSpacesIfNeeded(ctx, firstArg) +
          ctx.addLeft(firstArg, "(") +
          ctx.addRight(secondArg, ")")

      case Term.ApplyInfix(
          Term.ApplyInfix(_, booleanEither(_), _, List(firstArg)),
          booleanEitherOr(op2: Term.Name),
          _,
          List(secondArg)
          ) =>
        replaceOpWithComma(ctx, op2) +
          removeWhiteSpacesIfNeeded(ctx, firstArg) + ctx.addLeft(firstArg, "(") +
          ctx.replaceTree(firstArg, secondArg.syntax) + ctx.replaceTree(secondArg, firstArg.syntax) + ctx
          .addRight(secondArg, ")")
    }.asPatch + addImportsIfNeeded(ctx) {
      case Term.ApplyInfix(_, booleanSymbolOr(_) | booleanEither(_), _, _)
          if !ctx.tree.contains(scalazBooleanSyntaxImport) =>
        catsBooleanSyntaxImport
    }
  }

}

object ScalafixUtils {
  def addImportsIfNeeded(ctx: RuleCtx)(checker: PartialFunction[Tree, Importer]): Patch = {
    ctx.tree
      .collectFirst(checker)
      .map(importer => ctx.addGlobalImport(importer))
      .asPatch
  }

  def replaceOpWithComma(ctx: RuleCtx, op: Term.Name): Patch =
    ctx.replaceTree(op, ",") + removeWhiteSpacesIfNeeded(ctx, op)

  def removeWhiteSpacesIfNeeded(ctx: RuleCtx, op: Term): Patch = {
    ctx.tokenList
      .leading(op.tokens.head)
      .takeWhile(_.is[Whitespace])
      .map(ctx.removeToken)
      .asPatch
  }

  def wrapInParensIfNeeded(ctx: RuleCtx, t: Term): Patch = {
    for {
      head <- t.tokens.headOption
      if !head.is[Token.LeftParen]
      last <- t.tokens.lastOption
      if !last.is[Token.RightParen]
    } yield
      ctx.addLeft(head, "(") +
        ctx.addRight(last, ")")
  }.asPatch
}
