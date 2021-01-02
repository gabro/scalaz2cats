# scalaz2cats
Automatic migrations from scalaz 7 to cats 1.0.

> # ⚠️ Disclaimer
> This project is not intented for general use. It's very specific to my needs and it doesn't cover many edge cases.
> That said, PRs are welcome and I'll do my best to provide review and guidance if there's interest.
> If a general migration from Scalaz to Cats is needed in the community,
> feel free to [follow the issue in the Cats repo](https://github.com/typelevel/cats/issues/1762).

## Running

Make sure you add this to `plugins.sbt`

    addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.24")


If you first run `sbt`, you can run the following

    scalafix github:gabro/scalaz2cats/Scalaz2cats_v1_0_0

Otherwise, from the terminal, you can run this

    sbt ";scalafixEnable; scalafix github:gabro/scalaz2cats/Scalaz2cats_v1_0_0"


## Contribution guide
Follow the [guide on the Scalafix website](https://scalacenter.github.io/scalafix/docs/rule-authors/setup).
As a quick-start, you can run the tests by doing:

```bash
cd scalafix
sbt tests/test
```
