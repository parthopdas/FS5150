namespace global

open Xunit

[<assembly: CollectionBehavior(MaxParallelThreads = 1)>]

do()
