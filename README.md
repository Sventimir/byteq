
# byteq

This is an exercise in compiler/virtual machine implementation. The
main point is to demonstrate, how a VM execution may be safeguarded
using the type system in order to ensure that programs never
crash. Indeed, the VM implementation is "correct by construction",
which is to say that instructions and memory model are typed
statically, so that no type mismatch can occur at runtime.  Combined
with the absence of an explicit fail instruction, this ensures that
the VM cannot crash.

Unfortunately, there was not enough time to implement a parser and a
type checker, so only hard-coded example programs may be run at the
moment.

## Building

In order to build the project you'll need the Glasgow Haskell Compiler
(GHC) and the Haskell stack build system. Having installed that, enter
the project's root directory and type:

    $ stack build

The process will output the binary somewhere in `.stack-work`
subdirectory.  The exact path should appear near the end of the
output.

## Running

One can find the built binary in the `.stack-work` directory or simply
run:

    $ stack exec byteq-exe -- $program_arguments

The program recognises the following commands:

* `interp $prog` - compiles and executes the selected program
* `build $prog` - compiles the selected program and outputs bytecode
    to a file named `$prog.bc` in the current directory
* `run $prog` – given a path, loads the bytecode and executes it
* `dump $prog` – given a program name, compiles it and outputs
    resulting VM instructions in human-readable form; given a
    filename, loads the bytecode and outputs instructions

In order to run the unit tests, type:

    $ stack test

There's very little work done in terms of testing, partly because I
didn't have enough time, but also because the correctness of the code
is pretty much enforced by the type system. The only thing that could
potentially go wrong is bytecode generation and loading (as both these
operations work on untyped bytestrings). The VM and (to a lesser
extent) also the compiler are "correct by construction", as explained
below.

## Program's main features and implementation details

In the following sections I discuss some design choices that I made
and explain why I chose this particular design.

## Implementation language - Haskell

I chose this particular implementation language for two reasons. First
of all it happens to be the language that I know the best. But more
importantly, it's one of the few languages with a type system strong
enough that allows to ensure that no successfully compiled program can
ever crash the VM. Indeed, the stack type and the instruction type are
constructed in a way which guarantees that given the right initial
stack, each subsequent instruction will find the stack in a shape
which allows for its successful execution. The GHC also guarantees
that only the right stack can be fed into an instruction set. If
the GHC is not convinced that the given stack must be well-formed
for a particular instruction set – it'll refuse to compile the code.

Moreover, the syntax Directed Acyclic Graph (DAG) is also statically
typed, although the peculiar relationship between the language
expressions and VM instructions they translate into is not encoded in
the type system. For this reason, contrary to the VM, the compiler *is
not* correct by construction, that is given a well-formed DAG it can
fail to produce an executable bytecode. This is encoded in the
`compile` function's type, which can return a compilation error.

In fact, I think the only way a well-formed DAG can fail to compile is
if there are references to undefined variables. All other potential
compilation errors should never happen, but I didn't have enough time
to prove this claim to the compiler, which forced me handle those
potential errors as well. I think it should be possible to exclude
those errors by further refining the types of DAG nodes.

### Memory management

The VM's memory is represented as a stack of values. Every instruction
is effectively a (pure) function from stack to stack, i.e. it picks
one or more items from the top of the stack and then puts a number of
(other) items back on the top. There are some exceptions to this, namely
`PrintVal` instruction is not pure, as it outputs some text. Also the
`Dig` instruction reaches for values beneath the top.

During compilation, while accumulating a sequence of instructions, the
compiler maintains a model of the memory, including types and names
of all the values pushed there. Whenever the DAG being compiled wants
to assign a value to a variable, the value's representation (including
its type) is pushed onto the stack and annotated with a name
(String). Whenever the DAG wants to access a previously assigned
variable, the name is searched for on the stack and, if found, its
address (essentially a natural number in Peano encoding) is
computed. This address is stored within `Dig` instruction and at
runtime it is used to retrieve the value (which may be buried deep on
the stack). Original variable names are *not* included in the
resulting bytecode.

Note that variable addresses are not static in this model and each
reference to the same variable might produce a different address at
which the value is to be found.

Sadly, all the variables remain on the stack until the end of program's
execution. There is no garbage collection mechanism yet, although
one could be implemented relatively easily by analysing which instruction
is the last to use each variable. The instruction which performs a
variable reference is always `Dig` or `Dup`, so by annotating them
temporarily with variable names we could traverse a sequence of
instructions from right to left and after each `Dig` (or `Dup`)
instruction referencing a particular variable for the first time we
would insert instructions to remove the unneeded value from the
stack. Such a solution would ensure minimal runtime overhead at the
expense of somewhat longer compilation time.

### The bytecode

The bytecode is not executed directly by the VM, but rather it is
parsed into the internal representation (`Instr` type in `Data.Instr`).
This is necessary to ensure type-safety of the execution. It also has
the additional benefit that malformed bytecode will be rejected by the
VM before execution starts. This parsing and sanity checking has some
runtime overhead (essentially it increases the time of loading the
bytecode into memory). This is, however, a one-time overhead at the
start of execution. After this is done, parsed instructions are executed
quickly and safely. This may matter for long-running programs, because
once their execution successfully starts, they're guaranteed to eventually
terminate (barring infinite loops).

### Code layout

The program consists of several modules which will be described below.

* `app/Main.hs` is the program's entrypoint. It contains all the frontend
  logic such as argument parsing and error reporting. It's responsible
  for calling the right function to perform each task it might be given.

* `src/Data/DAG.hs` defines the syntax of the language of the VM. It
  consists of expressions and variable assignments, that the compiler
  translates to sequences of VM instructions.

* `src/Data/Type.hs` – this short module describes the type system of
  the VM. Notably, it links VM types to Haskell types using a phantom
  type parameter. This linking is essential for the type-safety of the
  whole system. Because Haskell can link VM's types to its own types,
  it can also make sure that VM instructions are type-safe.

* `src/Data/Stack.hs` models the memory of the VM. All the values produced
  and consumed by the VM are stored on the stack. Each instruction
  (except for `Push`) takes 1 or more values from the stack, combines them
  and returns another value to the top of the stack. There are no named
  variables in the VM internals, just the stack.

  The stack is also used during compilation and bytecode validation.
  The compiler maintains a model of the runtime stack filled with
  types of the variables than should be there at runtime. When a variable
  assignment takes place, the compiler annotates the top value with the
  variable's name. When a variable is referenced, the name is found on the
  stack model and its position is computed and stored in the bytecode
  as a sort of memory address.

  Note that the stack type is parametrised by the list of types of values
  that live there. This way virtually any possible stack has its unique
  type. By computing and comparing these types, GHC can statically ensure
  that a type mismatch can never occur inside the VM.

* `src/Data/Instr.hs` defines the VM's low-level instructions. Similarly
  to the stack values, instruction types are parametrised by lists of
  types on the stack before and after instruction's execution. This is
  how GHC given a stack and a sequence of instructions can determine
  whether they can be executed safely against the stack or not. It only
  compiles the code if it can be sure that this always is the case at
  runtime.

  Note how each instruction has assumptions about the stack's shape
  encoded in its own type. Also note how simple the `execInstr`
  function is. It is straightforward and elegant, although it lacks
  any error handling. This is because it's completely type-safe thanks
  to the precise and expressive typing of the stack and VM's
  instructions. This is because given the stack types, GHC can exclude
  any potential type errors, thereby lifting the burden of handling
  these errors from the programmer.

* `src/Data/Bytecode.hs` deals with serialization and deserialization
  of instructions. Paradoxically this is the longest and the most
  complex part of the system. It deals with untyped bytes, therefore
  everything is possible. There's a lot of error handling and pattern-
  matching involved in establishing that the bytecode we load from a
  file is actually type-safe. The loader is a little similar to the
  compiler in this, although it's work is significantly simpler,
  because it traverses a string of bytes, rather than a syntax graph.
  That said, there's far more room for error there than even in the
  compiler. However, similarly to the compiler it has to maintain a
  model of the stack in order to make sure that the instruction
  sequence it assembles is well-typed.

* `src/Control/Compiler.hs` contains the `compile` function which
  translates the syntax graph to a sequence of instructions.
  It maintains a model of the stack, which it uses for two purposes.
  For one thing, just like the bytecode loader, it needs to prove
  that generated instruction sequence is type-safe. Secondly, it
  also uses the stack model to track variables created by the program.
  This is how variable names appearing in the syntax graph are
  converted to stack positions, which essentially serve as memory
  addresses.


