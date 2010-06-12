bwp ("bwoop")

A simple tool to generate sound effects from scratch by
creating and combining waveforms in various ways.

Written in Haskell for GHC.

Language:
    bwp uses a simple "scripting language" to generate sounds.

    A script takes the form of a series of bindings of names
    to values. Expressions can refer to previously bound names
    but not recurse or refer to names that will be bound later.
    The basic form is this:

        name: expression;

    An expression can be a constant numeric value, corresponding
    to a graph/wave that has that value everywhere.

    An expression can be a function call:
        f{alpha=expr1,beta=expr2}
    (Assuming that there exists a function "f" that takes
    arguments "alpha" and "beta".) Functions can be partially
    applied, e.g. to pass the partially-applied function as an
    argument to another function; simply specify fewer arguments
    than required.

    An expression can be the negation of another:
        -expr

    An expression can be a combination of two values by
    addition, multiplication, or subtraction:
        expr1 * expr2
        expr1 + epxr2
        expr1 - expr3

    New functions can also be defined. This is done with a binding
    of the following form:
        f{alpha,beta}: alpha + beta;

    Some functions take data lists as arguments. These are lists of
    pairs of numbers; generally associating a duration with a value.
    Such a function is called like this:
        f{data=[(0,1),(2,3),(4,5)]}

    A number of built-in functions exist that, along with the
    constant waves, serve as basic building-blocks. These are:
        sine{freq}
        sawtooth{freq}
        triangular{freq}
        square{freq}

        exp_decay{speed}
        linear_decay{speed}

        linear_interpolation{data, initial}

        phase_shift{wave,shift}
        speed_shift{wave,speed}
        delay{wave,delay}
        clip{wave,min,max}

        random{seed,min,max}
