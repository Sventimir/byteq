
# byteq

This is an exercise in compiler/virtual machine implementation. The
main point is to demonstrate, how a VM execution may be safeguarded
using the type system in order to ensure that programs never
crash. Indeed, the VM implementation is correct by construction, which
is to say that instructions and memory model are typed so that no type
mismatch can occur at runtime.  Combined with the absence of an
explicit fail instruction, this ensures that the VM cannot crash.

## Building

In order to build the project you'll need the Glasgow Haskell Compiler
(GHC) and the Haskell stack build system. Having installed that, enter
the project's root directory and type:

    $ stack build
    
The process will output the binary somewhere in `.stack-work` subdirectory.
The exact path should appear near the end of the output.
    
## Running

One can find the built binary in the `.stack-work` directory or simply
run:

    $ stack exec byteq-exe -- $program_arguments
    
In order to run the unit tests, type:

    $ stack test
    
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
which allows for its successful execution.

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

This is actually why I love Haskell so much: it allows the programmer
to prove that some execution paths can never be taken and therefore
need not to be handled. In the absence of such as proof, however, GHC
requires the programmer to handle every possibility. This property is
not yet taken to the extreme like in some more modern languages –
Haskell still has exceptions (and some standard library functions
sadly do throw them) - but if a programmer refrains from throwing
them, there is a big chance none will ever occur.

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
to assign a value to a variable, the value is pushed onto the stack
and annotated with a name (String). Whenever the DAG wants to access a
previously assigned variable, the name is searched for on the stack
and, if found, its address (essentially a natural number in Peano
encoding) is computed. This address is stored within `Dig` instruction
and at runtime it is used to retrieve the value (which may be buried
deep on the stack). Original variable names are *not* included in the
resulting bytecode.

Note that variable addresses are not static in this model and each
reference to the same variable might produce a different address at
which the value is to be found.

Sadly, all the variables remain on the stack until the end of program's
execution. There is no garbage collection mechanism yet, although
one could be implemented relatively easily by analysing which instruction
is the last to use each variable. The instruction which performs a
variable reference is always `Dig`, so by annotating them temporarily
with variable names we could traverse a sequence of instructions from
right to left and after each `Dig` instruction referencing a
particular variable for the first time we would insert instructions to
remove the unneeded value from the stack. Such a solution would ensure
minimal runtime overhead at the expense of somewhat longer compilation
time.

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
