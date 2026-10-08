# bayests 0.0.0.9000

* **The shared R layer of bvartools, dfmtools and ucmtools, taken out of
  bvartools 1.0.0.** 180 functions moved: the generics of the workflow, the
  `modellist` and `expandingwindow` methods, reading and writing model files,
  the stored-model functions (`read_model_file()`, `read_model_folder()`,
  `map_models()`), the BayesTS command line runners and the model
  comparison functions. bvartools keeps its VAR and VEC models, their priors
  and analysis, the forecast pooling and external forecasts, and the
  `add_dummy_variables()` generic.

* The documentation of the 'modellist' and 'expandingwindow' methods no longer
  says that forecasts are never seeded per model: bvartools now derives the seed
  of a model's forecast from `object$model$seed`, so a seeded model forecasts the
  same on any number of workers. Only a model without a seed forecasts from the
  streams of the workers. Nothing in this package changes draws.

* **Renamed while unreleased.** Functions that read from disk start with
  `read_`, as R users expect: `read_model_file()` (was `open_model()`),
  `read_model_folder()` (was `open_models()`), `read_model_groups()` (was
  `list_models_in_hdf5()`) and the new generic `read_hdf5_model()`. The class
  of `read_model_file()` is `modelfile` (was `bvarfile`) and that of
  `read_model_folder()` is `modelfolder` (was `bvarfolder`). The options are `bayests.executable` (was
  `bvartools.bayests_executable`) and `bayests.size_warning` (was
  `bvartools.size_warning`). None of these were in a CRAN release.

* **`read_model_from_hdf5()` reads the models of any package the same way.**
  The class comes from `/model/rclass` or, for a file of the BayesTS command
  line, from the prefix of its algorithm (`Var`, `Vec`, `Dfm`, `Favar`, `Uc`).
  The defining package is loaded, and the file is handed to the new generic
  `read_hdf5_model()`, whose default reads the tree and calls
  `from_bayests_tree()`. The VAR and VEC reader becomes a `read_hdf5_model()`
  method of bvartools.

* `read_model_folder()` adds a `package` column to its manifest, and the workers of
  `map_models()` attach that package and this one rather than bvartools,
  with one BLAS thread each, as the other parallel steps already started
  them.

* **The plots and prints of the results every model package returns live
  here**: `plot()` of forecasts ('bvarprd', 'bvarprdlist'), impulse responses
  ('bvarirf') and variance decompositions ('bvarfevd'), and `print()` of time
  variation tests ('bvartimevar'). dfmtools returns these classes too, so it
  can plot them without bvartools. The class names stay as they are.

* `chain_diagnostics()` and `combine_models()` accept a model of any package:
  a list with an element `model` that is not a list of models.

* The helpers bvartools calls from its methods are exported under their
  dot-names, with `draw_error_bars()` for its forecast error plots, and
  documented in `?"bayests-developer"`.

* `add_predictive_loglik()` documents the generic only. How a draw is carried
  one period forward depends on the model, so the method for an expanding
  window of VAR and VEC models stays in bvartools with its documentation.
