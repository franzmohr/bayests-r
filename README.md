# bayests

The R infrastructure shared by the packages that run the samplers of the
[BayesTS](https://github.com/franzmohr/BayesTS) library:

- **bvartools**: vector autoregressive and error correction models;
- **dfmtools**: dynamic factor models and factor augmented VARs;
- **ucmtools** (planned): unobserved components models.

Each of them builds its own model objects. `bayests` holds what they have in
common, so that a model of one package is set up, estimated, stored and
compared the same way as a model of another. It estimates no model itself.

| Part | Functions |
| --- | --- |
| Generics of the workflow | `add_priors()`, `add_initial_values()`, `add_seed()`, `add_posterior_coefficients()`, `add_posterior_loglik()`, `add_posterior_forecasts()`, `add_forecast_input()`, `add_forecast_errors()`, `add_predictive_loglik()`, `add_sign_restrictions()`, `add_sign_zero_restrictions()`, `irf()`, `fevd()`, `time_variation_test()`, `expected_model_size()`, `write_to_hdf5()` |
| Model files | `read_model_from_hdf5()`, `read_bayests_tree()`, `write_bayests_tree()`, `from_bayests_tree()`, `from_hdf5()`, `list_models_in_hdf5()`, `read_models_from_folder()` |
| Stored models | `open_model()`, `map_draws()`, `open_models()`, `map_models()`, `model_files()` |
| Lists of models | the `modellist` and `expandingwindow` methods of every generic, `combine_models()`, `use_expanding_window()` |
| The BayesTS command line | `bayests_files()`, `bayests_posterior()` |
| Model comparison | `selection_criteria()`, `choose_best_model()`, `chain_diagnostics()`, `plot_predictive_loglik()` |

## For a model package

A package adds a model class by defining methods for the generics here. A
file it writes records its class in `/model/rclass` and the package in
`/model/rpackage`, and `read_model_from_hdf5()` loads that package and hands
the file to its `from_hdf5()` method, or reads the tree and hands it to its
`from_bayests_tree()` method. The internal helpers a method needs, such as
seeds, HDF5 writers and the pieces of the selection criteria, are exported
under their dot-names and documented in `?"bayests-developer"`.

## Installation

```r
# install.packages("remotes")
remotes::install_local("path/to/bayests-r")
```
