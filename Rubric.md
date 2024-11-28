# Project Rubric

## Correctness
- **Does it work? And is the project "complete"?**
  We will be looking to see whether the project achieves its goals and works as intended. During the course of your work, your mentor will help you refine your project goals.

- **How "difficult" was the project?**
  We will be looking for projects that include a significant amount of well-thought-out code and design. We would like to see evidence that you have stretched yourself in some way and have produced something more in-depth than, say, a standard data structure or algorithm.

## Design
Not all projects will demonstrate the same opportunities for design. The following are qualities that projects can use to demonstrate their knowledge of CIS 5520 design principles. Simple projects may lose points here because there may not be enough opportunity to encounter meaningful design decisions.

- **Decomposes the project into modules**. 
  Gives thought to constructing a reusable interface for at least one module (i.e., explicit export list, own type class, or clear indication that this should be a library). We are especially interested in the compositionality of the library operations — witnessed perhaps by Monoid or Applicative/Monad instances.

- **Defines appropriate data structures and uses them appropriately**. 
  In particular, the project employs `newtypes` and/or `datatypes` instead of built-in types when appropriate. The data structures used should lead to good running time (i.e., the code doesn't use `!!` for lists or include redundant traversals). Furthermore, the type structure should capture invariants.

- **Uses a nontrivial purely functional algorithm**. 
  In particular, the code does not put pure code in the IO Monad. The project may extend or adapt a homework problem or (better) develop or implement a new algorithm.

- **Uses abstractions covered in class** (e.g., Monoid, Functor, Applicative, any Monad other than IO). 
  This may be merely a use of some of the libraries we developed in class (e.g., State or Parser) or it may be as sophisticated as the use of monad transformers.

- **Demonstrates abstraction via higher-order functions or type classes**. 
  This also includes well-designed helper functions that identify common patterns. We would like to see uses of higher-order library functions, such as `map` or `fold`. Even better is a design that uses new higher-order functions to structure the program.

## Testing
Testing should be appropriate for the project and should be a convincing demonstration of the project's correctness (without relying on the demo). We particularly want to see code that was written or refactored to make it easier to test.

- **Regression tests**
- **Unit tests**
- **QuickCheck properties**

## Other Considerations
Other considerations may affect the final score of the project. These include:

- **Does the project follow style guidelines?**
- **Did the project require applying advanced features of Haskell** (profiling, type system features, etc.)?
- **Did all team members work together on the project?**
