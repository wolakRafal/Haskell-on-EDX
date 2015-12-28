Continuation passing style
---------------------------
You have (side effecting) action/function `f` with return value of type `a`

`f :: IO a`

 - We can change a function into continuation passing style by adding an extra parameter,
the continuation, that represents the "future" work that needs to be done
after this function terminates.
 - Instead of producing its result directly, the function will now apply the continuation to the result.

 Given a computation of type `Action`, a function that uses a continuation with result type a has the following type:

`(a -> Action) -> Action`

This type can be read as a function that takes as input a continuation function (a -> Action),
that specifies how to continue once the result of type `a` of the current computation is available.
An application `f c` of this type will call `c` with its result when it becomes available.
