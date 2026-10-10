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

* **bvartools is in Suggests, and the examples need its next version.** A
  model class comes from a package that builds on bayests, so the examples use
  the VAR models of bvartools. The bvartools on CRAN (0.3.0) predates bayests,
  so every example is guarded by
  `requireNamespace("bvartools", quietly = TRUE) && utils::packageVersion("bvartools") >= "1.0.0"`
  and is skipped until bvartools 1.0.0 is on CRAN. The tests do not need
  bvartools: they define a model class of their own.

* **\donttest{}** is used twice:
  * in `?folder_steps`, whose steps each read and write every model file of a
    folder: the timed part takes 2.3 s, the whole example 4.5 s;
  * in `?add_posterior_coefficients.modellist`, for the same estimation on two
    worker processes.

* **\dontrun{}** is used once, in `?bayests_files`, whose functions run the
  BayesTS command-line program on directories of model files. The program is
  not part of the package and is not installed on CRAN's machines.

* Examples and tests write only to `tempdir()`.
