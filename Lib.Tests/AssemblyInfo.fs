namespace global

open Xunit

// TODO: Remove this once we remove the Common.mb and related tests
[<assembly: CollectionBehavior(MaxParallelThreads = 1)>]

do()
