## Submission

This is a new submission.

bayests holds the infrastructure shared by the R packages that run the samplers
of the BayesTS C++ library: the generics of their model workflow, reading and
writing model files in BayesTS's HDF5 layout, lists of models and model
comparison. It estimates no model itself. It was taken out of bvartools, whose
next release (1.0.0) imports it and will be submitted once bayests is on CRAN.

## R CMD check results

0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... NOTE
  New submission

* checking top-level files ... NOTE
  Files 'README.md' or 'NEWS.md' cannot be checked without 'pandoc' being
  installed.

  The check machine had no pandoc on its path. Both files convert to HTML
  without errors with pandoc 3.8.3.

## Test environments

* Windows 11 x64, R 4.6.1 (ucrt), `R CMD check --as-cran`, twice:
  * with bvartools 0.3.0, the version on CRAN;
  * with the development version of bvartools 1.0.0, which imports bayests.
* GitHub Actions, `R CMD check` with the dependencies from CRAN, Status: OK on
  all five:
  * Ubuntu (x86_64-pc-linux-gnu): R-devel (2026-10-08 r90650), R 4.6.1 and
    R 4.5.3;
  * macOS (aarch64-apple-darwin23): R 4.6.1;
  * Windows (x86_64-w64-mingw32): R 4.6.1 (ucrt).

## Notes for the reviewer

* **The examples run on a model of the package's own.** Model classes come
  from the packages that build on bayests, so the package defines one small
  model for its examples, `create_normalmodel()`: a conjugate normal model of
  the mean with known variance, whose posterior draws are exact. Every example
  that sets up, estimates, stores or compares models runs on it, without any
  other package.

* **bvartools is in Suggests** for two examples only, those of the plot
  methods for impulse responses and variance decompositions
  (`?plot.bvarirf`, `?plot.bvarfevd`), whose objects the example model, having
  no dynamics, cannot produce. They are guarded by
  `nzchar(system.file(package = "bvartools")) && utils::packageVersion("bvartools") >= "1.0.0"`,
  since the bvartools on CRAN (0.3.0) predates bayests, and run once
  bvartools 1.0.0 is on CRAN. The guard asks for the version without loading
  the namespace: bvartools 0.3.0 registers plot methods of its own for these
  classes, which would replace those of bayests for the rest of the examples. The generics whose only methods are those of a
  VAR -- `irf()`, `fevd()`, `add_sign_restrictions()`,
  `add_sign_zero_restrictions()` and `time_variation_test()` -- have no
  example here; theirs are in bvartools.

* **\donttest{}** is used once, in `?add_posterior_coefficients.modellist`,
  for the same estimation on two worker processes.

* **\dontrun{}** is used once, in `?bayests_files`, whose functions run the
  BayesTS command-line program on directories of model files. The program is
  not part of the package and is not installed on CRAN's machines.

* Examples and tests write only to `tempdir()`.
