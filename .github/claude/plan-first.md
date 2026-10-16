# Plan-first template

Write the plan as ONE comment on the issue, with exactly these sections. Do not edit files, create
branches, or open pull requests. Read the code before writing; name real files and functions.

## Understanding
Restate the request in your own words and link the files and functions it touches. Say what is unclear.

## Proposed change
Per package (`sepsisfactors`, `sepsisfactorsdb`, `sepsisfactorsapp`): the files to change and what changes
in each. Prefer the idioms of the documentation of the packages involved; say so if something needs a
workaround, and why.

## Contract and schema impact
Does the results output, a method's output, or the database layout change? Which versions are bumped
(`RESULTS_OUTPUT_VERSION`, `method_output_version`, `SCHEMA_VERSION`) and which vignettes are updated?
Write "none" if none.

## Tests and verification
Which tests are added or changed, what you will run (`devtools::test()`, `devtools::check()`), and what
cannot be verified here (for example cluster runs).

## Risks and open questions
Numbered questions the maintainer must answer, each with your recommendation.

## Out of scope
What this change deliberately does not do.

End with: "Comment `@claude` with changes to revise this plan. Add the `auto-fix` label to implement it."
