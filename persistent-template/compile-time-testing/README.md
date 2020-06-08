This directory contains example projects to compile, for the purpose of testing reducing compilation time of Persistent models. Ideally the projects are varied, from intentional test cases (e.g. 10 models each with 100 fields) to real world projects.

The current projects are:

* `Mercury`. Copied from a production codebase, with modifications (mostly changing enums to `Text`). 42 models. Features UUID primary keys, composite primary keys, and timestamp fields.

The recommended testing procedure is:

### Dependencies

* [`bench`](https://hackage.haskell.org/package/bench), a command-line wrapper around [`criterion`](https://hackage.haskell.org/package/criterion)
* `uuidgen`, to generate a random file name.
* `ruby`, to run a script to aggregate timings.


### Procedure

1. Starting from `master`, build your example project. You want it such that future runs will have all of its dependencies built. Also add the `-ddump-timings` and `-ddump-to-file` flags so you can see where the generated file is:

```
stack build PROJECTNAME --ghc-options='-O0 -ddump-timings -ddump-to-file'
```

2. Find the location of the `.dump-timings` files:

```
find persistent-template/compile-time-testing/projects/PROJECTDIR/.stack-work -type f -name '*.dump-timings'
```

Copy the path to the module you want to check compilation data on. An example path is `persistent-template/compile-time-testing/projects/Mercury/.stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/TestPerformance.dump-timings`, but it will vary.

3. Benchmark:

```
mkdir RESULTS_DIR
bench --before="stack clean persistent-performance-test" "stack build persistent-performance-test --ghc-options='-O0 -ddump-timings -ddump-to-file'" --after="cp PATH_TO_TIMINGS_FILE RESULTS_DIR/`uuidgen`.dump-timings"
```

4. This benchmark will include the noise/overhead of calling GHC and compiling other files. To get module-specific data, use the `add-timings.rb` script to see how long compiling your specific module took.

5. Repeat steps 3–4 once or twice more with a new results directory. These times are your baseline to compare any changes against.

6. Make your change to `persistent-template`.

7. Compile your example project again.
8. Perform steps 3–4 to see how your change affects compilation speeds.


### TODO

* Improve the script to do better data analysis. Ideally it would use similar methods to Criterion, like an ordinary least squares regression, included an R^2 goodness of fit, standard deviation, etc.
* Simplify/script the procedure