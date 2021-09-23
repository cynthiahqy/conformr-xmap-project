# conformr

This is a work-in-progress.
Please follow me on twitter at [@cynthiahqy](https://twitter.com/cynthiahqy) for updates.

# Overview
`conformr` provides tools
for harmonising multiple data across classifications or statistics 
into a consistent validated dataset.

It is not uncommon in empirical social sciences to encounter "panel" data that has been reported using different standards in different time periods. 
Examples include occupation level labour statistics, or product level trade flows, whereby classification codes are updated periodically to reflect changes in category relevance. Over time categories may be added, removed or even split. 

In order to combine this kind of data into a single panel dataset, researchers must first harmonise the data across classification standards. In particular, researchers must make decisions on how to redistribute values when categories don't have a one-to-one correspondence. These decisions are often hidden in nested for-loops and case statements over each old-new category correspondence. The nature of nested loops make it difficult to review the dataset design, or even validate the data cleaning has been performed as intended.

`conformr` provides a matrix framework for applying transforming data between standards to:
- avoid loss or duplication of data when converting between classifications
- resolve conflicts between reported and calculated statistics
- improve reproducibilty and tractability of combined datasets by
  - facilitating pipelines of multiple sequential concordances
  - providing an tibble-based alternative to nested cases loops for handling one-to-one, one-to-many, and many-to-one correspondences

`conformr` is built upon the realisation that implicit in any transformation between classifications is an application of *weights* to the original values. For example, a transformation from 1 original category to 2 new categories, requires spliting the original value in 2, or alternatively, applying a weight of 0.5 to the original value twice (once each for the 2 new categories).

`conformr` removes the need for nested loops by explicitly stating the weights in a *data map*, which appends *weights* to the every old-new code pair. *data maps* can be thought of as an extension of standard concordance tables such as those provided in `{concordance}` and `{countrycode}`.

Planned features include:
* single step transformation of numeric data between classifications across one-to-one, one-to-many and many-to-one cases (with validation) using data maps: `convert()`
* helpers for verifying and creating valid data maps:
* helpers for identifying discrepancies between reported and calculated statistics (e.g. `compare` reported category totals vs. calculated sum of category records)
* single step corrections of discrepancies (e.g. `distribute` difference between category members, or `replace` reported totals with calculated sums)
