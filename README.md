# Update Monad

The update monad is a generalized (and pretty abstract) monad which can be specialized to implement any of Reader,
Writer, or State! Check out [this paper](https://danelahman.github.io/papers/types13postproc.pdf) for a description.

This repo implements both the update monad transformer (`UpdateT`) and an
experiment called `FreeUpdateT` which forgoes the traditional requirement of
using a monoid as the 'action' of the update monad by substituting a free
monoid (i.e. `[action]`). It also defers the choice of an
`act :: action -> state -> state` function until the last second when the monad
is run (rather than using a typeclass); this conveniently allows you to
substitute out the `act` function for testing or alternate behaviour without
needing to rewrite or transform the monadic values themselves. Nifty!

Yes; I know I could implement `Update` in terms of `UpdateT Identity`; but implementing
them separately is clearer for people learning how it works.

