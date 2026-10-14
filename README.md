# bayests

[![CRAN status](https://www.r-pkg.org/badges/version/bayests)](https://cran.r-project.org/package=bayests)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/bayests)](https://cran.r-project.org/package=bayests)
[![Total downloads](https://cranlogs.r-pkg.org/badges/grand-total/bayests)](https://cran.r-project.org/package=bayests)
[![R-CMD-check](https://github.com/franzmohr/bayests-r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/franzmohr/bayests-r/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/franzmohr/bayests-r/graph/badge.svg)](https://app.codecov.io/gh/franzmohr/bayests-r)
[![License: GPL (\>= 2)](https://img.shields.io/badge/license-GPL%20%28%3E%3D%202%29-blue.svg)](https://www.gnu.org/licenses/gpl-2.0)

[![GitHub Sponsors](https://img.shields.io/badge/Sponsor-%E2%9D%A4-ea4aaa?logo=github-sponsors&logoColor=white)](https://github.com/sponsors/franzmohr)
[![Buy me coffee & AI tokens](https://img.shields.io/badge/Buy%20me%20coffee%20%26%20AI%20tokens-FFDD00?logo=buymeacoffee&logoColor=black)](https://buymeacoffee.com/franzmohr)

The R infrastructure shared by the packages that run the samplers of the
[BayesTS](https://github.com/franzmohr/BayesTS) library:

- **bvartools**: vector autoregressive and error correction models;
- **dfmtools**: dynamic factor models and factor augmented VARs;
- **ucmtools** (planned): unobserved components models.

Each of them builds its own model objects. `bayests` holds what they have in
common, so that a model of one package is set up, estimated, stored and
compared the same way as a model of another. It estimates no model for
analysis: the one it defines, `create_normalmodel()`, a conjugate normal model of
the mean with known variance, is there to show the workflow without a model
package, and the examples run on it.

| Part | Functions |
| --- | --- |
| Generics of the workflow | `add_priors()`, `add_initial_values()`, `add_seed()`, `add_posterior_coefficients()`, `add_posterior_loglik()`, `add_posterior_forecasts()`, `add_forecast_input()`, `add_forecast_errors()`, `add_predictive_loglik()`, `add_sign_restrictions()`, `add_sign_zero_restrictions()`, `irf()`, `fevd()`, `time_variation_test()`, `expected_model_size()`, `write_to_hdf5()` |
| Model files | `read_model_from_hdf5()`, `read_bayests_tree()`, `write_bayests_tree()`, `from_bayests_tree()`, `read_hdf5_model()`, `read_model_groups()`, `read_models_from_folder()` |
| Stored models | `read_model_file()`, `map_draws()`, `read_model_folder()`, `map_models()`, `model_files()` |
| Lists of models | the `modellist` and `expandingwindow` methods of every generic, `combine_models()`, `use_expanding_window()` |
| An example model | `create_normalmodel()` and its methods |
| The BayesTS command line | `bayests_files()`, `bayests_posterior()` |
| Model comparison | `selection_criteria()`, `choose_best_model()`, `chain_diagnostics()`, `plot_predictive_loglik()` |

## For a model package

A package adds a model class by defining methods for the generics here. A
file it writes records its class in `/model/rclass` and the package in
`/model/rpackage`, and `read_model_from_hdf5()` loads that package and hands
the file to its `read_hdf5_model()` method, or reads the tree and hands it to its
`from_bayests_tree()` method. The internal helpers a method needs, such as
seeds, HDF5 writers and the pieces of the selection criteria, are exported
under their dot-names and documented in `?"bayests-developer"`.

## Installation

```r
# install.packages("remotes")
remotes::install_local("path/to/bayests-r")
```
