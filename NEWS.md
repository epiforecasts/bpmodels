# bpmodels 0.2.0

## Documentation

* `chain_sim()`'s help file has been updated with more details. In particular,
we describe in detail how to specify the `serial` argument as a function. We 
have also added more examples.

* A new vignette describing how to project COVID-19 incidence with `chain_sim()`
has been added and can be accessed on the 
[bpmodels website](https://epiverse-trace.github.io/bpmodels/) under "Articles".

* The README's "quick start" section has been updated with what was 
previously the introduction vignette.

## Minor functionality change

* `chain_sim()` now throws a warning, instead of an error, when `tree` is set 
to `FALSE` with `serial` also specified. Providing a serial interval implicitly
means you want the tree of transmissions to be simulated, so `chain_sim()`
internally sets `tree = TRUE` and throws a warning explaining what happened. 
This behaviour should not break any simulations with previous versions 
with `bpmodels`, but if it does, please submit an issue. 
To remove the warning, the user should explicitly set `tree = TRUE` when 
they specify `serial`. 

# bpmodels 0.1.9999

* faster, vectorised chain simulations

# bpmodels 0.1.0

* initial release
