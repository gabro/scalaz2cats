lazy val V = _root_.scalafix.Versions
// Use a scala version supported by scalafix.
scalaVersion in ThisBuild := V.scala212

lazy val rules = project.settings(
  libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % V.version
)

lazy val input = project.settings(
  scalafixSourceroot := sourceDirectory.in(Compile).value,
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.15"
)

lazy val output = project.settings(
  libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0-MF"
)

lazy val tests = project
  .settings(
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % V.version % Test cross CrossVersion.full,
    buildInfoPackage := "fix",
    buildInfoKeys := Seq[BuildInfoKey](
      "inputSourceroot" ->
        sourceDirectory.in(input, Compile).value,
      "outputSourceroot" ->
        sourceDirectory.in(output, Compile).value,
      "inputClassdirectory" ->
        classDirectory.in(input, Compile).value
    )
  )
  .dependsOn(input, rules)
  .enablePlugins(BuildInfoPlugin)
