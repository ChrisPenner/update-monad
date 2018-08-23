# Update Monad

The update monad is a generalized (and pretty abstract) monad which can be specialized to implement any of Reader,
Writer, or State! Check out [this paper](https://danelahman.github.io/papers/types13postproc.pdf) for a description.

This repo implements both the update monad transformer (`UpdateT`)  and an experiment called `FreeUpdateT` which
forgoes the traditional requirement of using a monoid as the 'action' of the update monad by substituting a free monoid
(i.e. `[action]`). It also defers the choice of an `act :: state -> action -> state` function until the last second
when the monad is run (rather than using a typeclass); this conveniently allows you to substitute out the `act` function for testing or alternate
behaviour without needing to rewrite or transform the monadic values themselves. Nifty!

I also included the State monad implementation over FreeUpdateT as MonadState
implementation with the action specialized to `s` with `uRunStateT` which uses
`flip const` as the action application which effectively employs the `Last s`
monoid to emulate the State Monad.


Check out `src/app/Main.hs` to see a few small examples of me testing stuff out.
