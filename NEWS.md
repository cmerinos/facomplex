# facomplex 2.0.0 (2026-11-11)

## New features

### Bootstrap inference for factorial simplicity indices

* New function `bootLoadings()` performs non-parametric bootstrap of factor
  loadings for EFA and ESEM models fitted with **lavaan**. It returns a
  `facomplexBoot` object that can be reused across all `ci*()` functions
  without refitting the model.

* New function `ciHofmann()` returns asymmetric bootstrap confidence
  intervals for Hofmann's (1977) complexity index at the item level
  (`CHof`, `CHof.R`, `CHof.S`), the factor level (`Chof`), or both,
  via `level = c("item", "factor", "both")`.

* New function `ciBSI()` returns bootstrap confidence intervals for
  Bentler's (1977) Simplicity Index.

* New function `ciLSI()` returns bootstrap confidence intervals for
  Lorenzo-Seva's (2003) Loading Simplicity Index.

* New function `ciSimload()` returns bootstrap confidence intervals for
  Fleming's (2003) indices: `TSFI` (matrix level), `SFI` (factor level),
  and `IFS` (item level).

* New function `ciSSindices()` returns bootstrap confidence intervals for
  the target-based simplicity indices `SStarget`, `SSntarget`, and
  `SSratio`, at the matrix or factor level.

### Factor alignment

* Each bootstrap replica is aligned to the full-sample solution via
  Tucker congruence followed by optimal assignment (Hungarian algorithm,
  via `clue::solve_LSAP()` when available; greedy fallback otherwise).
  Sign reflection is corrected after permutation. This resolves the
  rotational indeterminacy inherent to EFA/ESEM bootstrap and ensures
  that per-item and per-factor indices are computed on comparable
  columns. See `?bootLoadings` for details.

### Target specification

* `bootLoadings()` accepts the target structure in three formats: a 0/1
  matrix (1 = expected loading, 0 = cross-loading), a 0/NA matrix
  (lavaan convention: NA = free, 0 = fixed to zero), or a named list
  (the `items_target` format used by `simload()`). A single internal
  parser normalizes all three.

### Confidence intervals

* Two asymmetric CI methods are available for all `ci*()` functions:
  percentile (`type = "perc"`, default) and bias-corrected
  (`type = "bc"`). The confidence level is controlled via
  `conf.level` (default `0.95`) at the `bootLoadings()` stage and
  inherited by the `ci*()` functions; it can be overridden per call.

## Improvements

* Progress reporting in `bootLoadings()` when `progress = TRUE`, printed
  every 10% of the bootstrap replicates.

* Optional parallelization via `boot::boot()` (`parallel = "multicore"`
  or `"snow"`, with `ncpus`).

* Failed replicas (non-convergence, Heywood cases) are caught and
  reported as `NA`, with a warning indicating the number of failures.
  Confidence intervals are computed from valid replicates only.

* `print.facomplexBoot()` summarizes engine, rotation, item and factor
  counts, valid replicas, and CI settings.

## Internal changes

* New file `R/facomplex-internal.R` centralizes:

  - `%or%`, a null-coalescing operator (named to avoid conflict with
    `base::%||%` introduced in R >= 4.4.0).
  - `tuckerCongruence()`, `alignFactors()`, `prepareTarget()`,
    `reconcileTarget()`, `fitFA()`, `extractLoadings()`, `computeCI()`,
    `buildRow()`.

* `bootLoadings()` accepts `engine = c("EFA", "ESEM")` and delegates to
  `lavaan::efa()` or `lavaan::sem()` through a unified `fitFA()` helper.

## Dependencies

* `boot` and `lavaan` are now in `Imports`.

* `clue` is in `Suggests` (optional; a greedy fallback is used if
  unavailable).

## Testing

* New unit tests cover: `bootLoadings()` output structure,
  `ciHofmann()` at item and factor levels, `ciSimload()` and
  `ciSSindices()` target requirement, and error handling for invalid
  inputs.

## Known limitations

* Factor alignment corrects permutation and sign reflection, but not
  local optima of the rotation algorithm. If a replica converges to a
  genuinely different structure, the congruences will be low and the
  CI will reflect this (wide intervals). This is valid information, not
  a defect.

## Notes

* The bootstrap-based `ci*()` functions are intended for use when the
  full dataset is available. If only the loading matrix is available,
  the original point-estimate functions remain the appropriate tools.

## Versioning note

* `2.0.0` bump reflects the
  significance of the new inference framework (bootstrap + factor
  alignment + reusable `facomplexBoot` object) rather than an
  incompatible change.
  
# facomplex 1.0.0 (2026-07-31)

* Minor corrections in documentation and references.
* Fixed a typo in the package title in README.
* Modified `entropyFL()`: several parameters, type `exp` and `conf`.
* First stable CRAN release.
* Hofmann(), now 3 versions: original, 1-0 scaled for complexity, 1-0 scaled for simplicity
* All core functions for factor complexity analysis are now stable.

# facomplex 0.0.3 (2026-07-25)

* First CRAN submission accepted.
* Added missing `@return` tag to `print.KC` documentation.

# facomplex 0.0.2 (2026-07-20)

* Fixed examples in `plotFacomplex` to avoid errors in `\donttest{}`.
* Corrected `Description` field format in `DESCRIPTION`.

# facomplex 0.0.1 (2026-06-23)

* Initial development version.
* Core functions: `simload`, `BSI`, `Hofmann`, `KC`, `LSIglobal`, `plotFacomplex`, `profileFacomplex`, `SSindices`.