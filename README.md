# scalaz2cats
Automatic migrations from scalaz 7 to cats 1.0.

> # ⚠️ Disclaimer
> This project is not intented for general use. It's very specific to my needs and it doesn't cover many edge cases.
> That said, PRs are welcome and I'll do my best to provide review and guidance if there's interest.
> If a general migration from Scalaz to Cats is needed in the community,
> feel free to [follow the issue in the Cats repo](https://github.com/typelevel/cats/issues/1762).

## Contribution guide
Follow the [guide on the Scalafix website](https://scalacenter.github.io/scalafix/docs/rule-authors/setup).
As a quick-start, you can run the tests by doing:

```bash
cd scalafix
sbt tests/test
```
