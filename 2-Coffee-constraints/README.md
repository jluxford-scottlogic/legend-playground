## Coffee Constraints: Example 2

This will be moving onto constraints and their use within classes, all of these constraints will be added onto the `Company` class although will also make use of the `CompanyBeans` association.

### Testing

To test your code throughout example 2 we will be using a self referential mapping that has pre-built tests. To use this you can add the `2_Coffee.pure` code into the bottom of your text file (do not overwrite your code from section 1 simply add it in as an extra) on your legend studio. Attempt to compile this, in the case that it fails to compile you might need to change some of the class values to match names exactly to what is given in the mapping.

### Constraint 1

The first constraint is going to be a basic test to make sure that all of the Ids are positive, you will likely need this [link](https://legend.finos.org/docs/reference/released-functions) for access to functions that will allow for easier constraints. One thing to note is that you can also use arrow notation to be able to create functions:

This looks like the following, instead of:

`function(argA)`

you can do:

`argA -> function()`

which follows a more functional style that allows you to chain together functions in a more pleasing way. Take for example:

`h(f(g(argA), argB), argC)`

this could instead be written as:

`argA -> g() -> f(argB) -> h(argC)`

which can be clearer. Attempt to use this arrow notation for these constraints where you can as good practice. For constraints and derived properties to access the class you are currently working in you will need to use `$this.Property`.

For the tests provided this should then cause one of the tests to fail and in the `Test Result` output it should fail due to a `constraint violation`.

### Constraint 2

The second constraint is going to be more complicated and will be that any company that handles `Raw` `Bean`s should be a `Roaster`, this might require the [if](https://legend.finos.org/docs/reference/released-functions#if) function to add the constraint easily. Some functions will naturally compile to undo arrow notation, the `if` statement is one such example. This is because the arrow notation is designed for functional styles while an `if` statement being specifically for imperative code means that it will automatically change if typed in arrow notation. Try the following constraint for example and notice that it will automatically remove the arrow notation if you convert between ui and text mode:

`true -> if(|false, |true)`

will convert to:

`if(true, |false, |true)`

If you look at the type signature of the `if` statement you will notice that it is slightly strange:

`if<T>(test: Boolean[1], valid: Function<{->T[m]}>[1], invalid: Function<{->T[m]}>[1]): T[m]`

in that it has these `Function<{->T[m]}>[1]` type signatures which denotes 1 (`[1]`)  `Function` with type signature `{->T[m]}`. This `{->T[m]}` means it is a function which takes in no arguments and returns a `T[m]` as output. The standard definition of a `Function` is a chain of types separated by `->` which means that it takes in inputs of that type to return the final value. For example a `length` function might be written as having type `Function<{String[1]->Integer[1]}>` which means it takes in a `String` and returns an `Integer`. This is effectively the same as the standard `length(str: String[1]): Integer[1]` type signature but implies you can also provide a lambda or anonymous function. This is where you create a function in place. You will notice above that in the `if` statement we used a `|true` and `|false` instead of `true` and `false`, the `|` is notation for creating a lambda so for example we could do the following:

`x | $x + 1`

which would be a function that adds one to a value. In the case of the `if` statement we don't have an input to the lambda so we just use an empty `|`. Try using an `if` statement to add in the constraint that any company that handles `Raw` beans should be a `Roaster`.

## Constraint 3

The final constraint which should bring all of the above together is to create a constraint such that all of the `BagId`s for the `Bean`s should contain the `CompanyId` of any `Company` that has ownership of those `Bean`s.