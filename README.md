# conformr
`conformr` provides tools
for harmonising multiple data across classifications or statistics 
into a consistent validated dataset.

It aims to help dataset curators:
- avoid loss or duplication of data when converting between classifications
- resolve conflicts between reported and calculated statistics
- improve reproducibilty and tractability of combined datasets by
  - facilitating pipelines of multiple sequential concordances
  - providing an matrix-style alternative to nested cases loops for handling one-to-one, one-to-many, and many-to-one correspondences

Planned features include:
* single step transformation of numeric data between classifications across one-to-one, one-to-many and many-to-one cases (with validation): `convert`
* helpers for verifying and creating weighted correspondence tables for use in with `convert`: `check_cd_`, `make_cd_`
* helpers for identifying discrepancies between reported and calculated statistics (e.g. `compare` reported category totals vs. calculated sum of category records)
* single step corrections of discrepancies (e.g. `distribute` difference between category members, or `replace` reported totals with calculated sums)
