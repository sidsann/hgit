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
  - It would easily follow to implement a few plumbing commands which we are certain to already have coded up for either functionality or testing purposes e.g. _git cat-file -p hashcode_

## Object Model & Workflow

User calls _git init_ which creates a _.hgit_ directory in the current directory. This directory will initially contain:
- A file titled _HEAD_, which will contain the relative file path e.g. "refs/heads/head", to the file that contains the object ID or OID for the commit object currently being pointed to by _HEAD_ e.g. the branch that the user is currently on.
- An objects directory, which will be two levels deep, the first level will contain directories named using the first two characters of the hexadecimal string (OID) formed from the SHA-1 hash of each file that's being tracked by hgit. The second level will contain files named using the latter 38 characters (An SHA-1 hash is 160 bits, and hexadecimal characters can each represent 4 bits since they're base-16, so 160/4 = 40 characters). The second level files will contain the serialized content of all objects: trees, commits, or blobs.
  - A commit object will contain the OID for it's root tree, the OID of the parent/s commit (no parent if root commit), the author with their name, email ID, and a timestamp of when the commit was made (which will be set by _git config user.name "name"_ and _git config user.email "email"_)
  - A tree object will contain data on either blobs or other trees representing subdirectories. Both object types will have their associated OID and file or directory names.
  - A blob object will simply contain serialized file content
- A refs directory, which will contain a _heads_ directory that will contain files, one for each local branch, named using that branch's name and its content will just be the OID for that branch, meaning the commit being pointed to by that branch

This directory will eventually also contain a file titled _index_ which will contain binary data on each file that is staged (it must have been staged via _git add_), with data on each file including the OID or SHA-1 hash and the path to the file from the root directory

__All of our implemented commands will rely on this object model, and they will not use any additional type of file or formatting to achieve our desired functionality.__ 

## Detailed Command Definitions incl. flags, args, etc.
**First block of commands to implement:**
  - hgit init: Will create the .hgit directory in the directory from where this command is given with an empty HEAD file, an empty objects directory, refs directory will just contain heads directory which will just contain an empty file called head
  - hgit add _______: Will update the index file with the added files, this will involve hashing the added file, seeing if it is already tracked and comparing the hashes, if same, then nothing more to do, if different, then update the hash and write to the objects directory with this file's contents (we're going to ignore optimizing here, we can later check to see if the existing hash is pointed to by any commit, if not we can delete it, maybe using a garbage collection thread which routinely checks for such blobs that are no longer connected to any commit or index)
    - Versions to support: 
      - hgit add filename: as expected (can support only single file name or recursively support many file names here)
      - hgit add -u: all tracked files in the entire working tree are updated
      - hgit add . : add all files in current directory and in subdirectories including hidden files and directories starting with a . (except for .hgit of course)
  - hgit commit: Create a new commit containing the current contents of the index and the given log message describing the changes. This means that a new commit object will be created and its parent will be the current commit object pointed to by HEAD, and its root tree will be created containing all the blobs or trees for the root dir of the project. This will involve DFS with and from the lowest level dir up we compare OIDs with what's in the objects directory, if it already exists, just point to it, otherwise create it. Keep in mind that this step will not be adding any new blob files (unless we support that option later) so it will only be adding new tree objects (if any) and a commit object.
    - Versions to support:
      - hgit commit: requires subsequent prompting for commit msg
      - hgit commit -m "commit msg" 
      - hgit 
  - hgit branch:
  - hgit checkout:
  - hgit commit:
  - hgit log: 
  - hgit status:
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

