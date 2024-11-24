# Haskell Git (hgit)

## TL;DR 
A lightweight, pure Haskell library implementing core Git functionality for local-only repositories.

## Module organization

Haskell packages typically divide their source code into three separate places:

  - The bulk of your code should be developed as a reusable library in 
    modules in the `src` directory. We've created [Lib.hs](src/Lib.hs) 
    for you to get started. You can add additional modules here.
    If you do add new modules to this directory you should list them
    in the cabal file: `project-cis5520.cabal`.
  
  - The entry point for your executable is in [Main.hs](app/Main.hs). 
  
  - All of your test cases should be in [the test directory](test/Spec.hs).

## Building, running, and testing

This project compiles with `stack build`. 
You can run the main executable with `stack run`.
You can run the tests with `stack test`. 

Finally, you can start a REPL with `stack ghci`.

## Importing additional libraries

This project is designed to run with stackage: you can easily use any library
in https://www.stackage.org/lts-22.32 by adding an entry to the
`build-depends` list of the `common-stanza` in the cabal file. If you want to
use a library that is not on stackage, you'll need to update the common-stanza
*and* add information to `stack.yaml` about where to find that library.

## High-level Overview

Git supports a number of commands, easily over a 100, of these, less than half are considered porcelain commands, high-level commands which most Git users are likely to be familiar with. The rest are known as plumbing commands and represent low-level operations which are most often used for scripting. For our project, we will implement a subset of the porcelain commands:

- First block of commands to support:
  - hgit init
  - hgit add
  - hgit commit
  - hgit branch
  - hgit checkout
  - hgit commit
  - hgit log
  - hgit status: simplest implementation will just compare hashes (object ID; OID) for tracked files, if different, then it has been modified
- Second block of commands to support:
  - hgit merge (highest priority after finishing first block)
  - hgit clean
  - hgit diff (diffs are calculated dynamically, when requested, so not necessary for core functionality)
    - hgit rebase (uses diffs to reconstruct branch by replaying diffs from another branch onto the current branch creating a simpler git history)
    - hgit cherry-pick (uses diffs to replay changes from a range of commits onto current head, very similar functionality to rebase, just more fine-grained)
    - hgit range-diff
  - hgit switch (subset of functionality offered by hgit checkout)
  - hgit reset, hgit restore, hgit revert
  - hgit rm
  - hgit mv
  - hgit help

## Object Model & Workflow

## Detailed Command Definitions incl. flags, args, etc.

## High-level Implementation Details Per Command

## Type-Definitions in Haskell to support Object Model and General Implementation

## Directory Structure and Abstractions, or Plan to Minimize Redundancies in Function Definitions

## Project Testing

Refer to [How to Specify It\!](https://research.chalmers.se/publication/517894/file/517894_Fulltext.pdf) for further details on the following testing methodologies. Each testing methodology is also defined within this specification using text taken verbatim from the aforementioned document.

### Validity Testing

Validity testing consists of defining a function to check the invariants of your data types, writing properties to test that your generators and shrinkers only produce valid results, and writing a property for each function under test that performs a single random call, and checks that the return value is valid.

- Test Idea
- Test Idea 2
- Etc.

### Postcondition Testing

A postcondition tests a single function, calling it with random arguments, and checking an expected relationship between its arguments and its result.

### Metamorphic Testing

A metamorphic property tests a single function by making (usually) two related calls, and checking the expected relationship between the two results.\*

### Inductive Testing

Inductive properties relate a call of the function-under-test to calls with smaller arguments. A set of inductive properties covering all possible cases together test the base case(s) and induction step(s) of an inductive proof-of-correctness. If all the properties hold, then we know the function is correct–inductive properties together make up a complete test.

### Model-based Testing

A model-based property tests a single function by making a single call, and comparing its result to the result of a related “abstract operation” applied to related abstract arguments. An abstraction functions maps the real, concrete arguments and results to abstract values, which we also call the “model”.

