# bayests 0.0.0.9000

* **The shared R layer of bvartools, dfmtools and ucmtools, taken out of
  bvartools 1.0.0.** 180 functions moved: the generics of the workflow, the
  `modellist` and `expandingwindow` methods, reading and writing model files,
  the stored-model functions (`open_model()`, `open_models()`,
  `map_models()`), the BayesTS command line runners and the model
  comparison functions. bvartools keeps its VAR and VEC models, their priors
  and analysis, the forecast pooling and external forecasts, and the
  `add_dummy_variables()` generic.

* **Renamed while unreleased.** The class of `open_model()` is `modelfile`
  (was `bvarfile`) and that of `open_models()` is `modelfolder` (was
  `bvarfolder`). The options are `bayests.executable` (was
  `bvartools.bayests_executable`) and `bayests.size_warning` (was
  `bvartools.size_warning`). None of these were in a CRAN release.

* **`read_model_from_hdf5()` reads the models of any package the same way.**
  The class comes from `/model/rclass` or, for a file of the BayesTS command
  line, from the prefix of its algorithm (`Var`, `Vec`, `Dfm`, `Favar`, `Uc`).
  The defining package is loaded, and the file is handed to the new generic
  `from_hdf5()`, whose default reads the tree and calls
  `from_bayests_tree()`. The VAR and VEC reader becomes a `from_hdf5()`
  method of bvartools.

* `open_models()` adds a `package` column to its manifest, and the workers of
  `map_models()` attach that package and this one rather than bvartools,
  with one BLAS thread each, as the other parallel steps already started
  them.

* `chain_diagnostics()` and `combine_models()` accept a model of any package:
  a list with an element `model` that is not a list of models.

* The helpers bvartools calls from its methods are exported under their
  dot-names and documented in `?"bayests-developer"`.
