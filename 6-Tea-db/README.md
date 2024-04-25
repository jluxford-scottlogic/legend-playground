## Tea Mappings: Example 6

Copy the `6_Tea.pure` text into your legend text editor to try this example, for now ignore the `interim` folders as these will be handled at a later point. This will be using some new `Tea` classes as well as a `h2` schema. The `h2` schema can only be viewed in text mode so it is worth having a quick run through of how the schema is designed. It will be defined using a standard SQL setup except there will be some extra lines at the bottom, specifically the `Join`s which will be used within the mappings. These somewhat mirror a normal SQL `Join` except it doesn't identify a table it instead references the operation of joining itself. This means that when you call the given join operation (for example `TeaLocation`) this will then action the `Join` between `Tea.location_id` and `Location.id`. However because this is an operation rather than a table you can then chain them for doubley linked tables, for example you can chain the `TeaDrink > BobDrink` to do both join operations.

### Testing

To test your code we will use some pre-defined tests which will take in a given JSON and expect an output JSON. Once the mappings are complete all of the tests should then pass.

# TODO

### Basic Mappings

First adjust all of the mapped classes to have their corresponding basic `Id`s. Once this is done the tests should now have all of the `class_id` tests pass and when you go through adjusting the association mappings the tests will then pass.

### Location

Adjust the `Location` mapping to convert from the `SQL` table to the `Location` class, this will require using a `Join` operation which uses an `@` symbol to get the association to work. This should then get the `location_basic` test running.

### Boba

Adjust the `Boba` mapping to convert from the `SQL` table to the `Boba` class, this will require using a linked join to be able to traverse the `Drink` table in the `SQL`. This should then have the `boba_basic` test running.

### Tea

Adjust the `Tea` mapping to convert from the `SQL` table to the `Tea` class. For now ignore the `TakenWithMilk` and `CanBeIced` properties as we will be handling them in the next section. Once `tea` is complete the full tests should then pass but the agg tests won't.

### Tea: the Cold stuff

Now try and adjust `TakenWithMilk` and `CanBeIced` to make use of the database values such that `TakenWithMilk` is true if all of the `milk` values for that teas drinks is `1`, while `CanBeIced` is true if any of the `ice` values are `1`. Once this is complete you should notice that the `agg` tests still fail, take a look at the `JSON` output and have a look at what is going wrong. (Ignore the `()` at the end of the properties for now as we'll be covering that in a bit.)

Having compared the expected output to the actual output you might notice that you end up with some extra instances of `Tea`, which ones do we end up with duplicates for and do the booleans all match up to what you might expect?

With the most common implementation you probably notice that whenever there is more than one drink with a certain type of tea then that tea is duplicated. Try updating the `TakenWithMilk` to return a static value and notice what changes within the output. Now try replacing `CanBeIced` with a static value as well and look at the result. Overall it is noticed that whenever we reference a new `Join` operation it treats this as a multiplicty on the condition that the resulting table has more than one of the required value. This means that for the `TakenWithMilk` and `CanBeIced` properties as we specified that they are of size `[0..1]` it has an inconsistent multiplicty with the resulting `Join` table. However due to it being a `Join` table and the mapping outputting a list it passes this duplication back up the chain and ends up making a duplicate of the `Tea` class that has the differing `TakenWithMilk` and `CanBeIced` property.

A large part of this is due to the aims of `Legend` and what it is designed to do, the process of deciding whether a certain tea can be iced or needs milk is an aggregation of the data rather than simply a mapping. This is because it is attempting to reduce the information provided within the `Drink` table to a single value rather than simply mapping the values that appear. This means that this logic ideally shouldn't be happening within the mapping at all, instead let's take a slightly different approach to this. First remove your static implentations of `TakenWithMilk` and `CanBeIced` within the mapping as any dangling references will cause issues in the future. Next add a couple of new properties to the `Tea` class (the naming is up to your discrestion but could be something like `MilkDrinkList` and `IcedDrinkList`) that are strings (or booleans) with a multiplicity of `[0..*]`. Now we will remove the `TakenWithMilk` and `CanBeIced` properties and add in two `Derived Properties` with matching property names, both still being booleans with multiplicity `[0..1]` and base values of `true`. This should then compile, in the case that it doesn't you likely have some dangling references to `TakenWithMilk` or `CanBeIced` in the mapping which you need to remove, that or you have incompatible types with your base values. Derived properties let you make a new property out of information that is on the class, this is one way of having some amount of post processing or aggregation effect on the class by having one property be constructed from other ones. Now we want these new properties to match the above logic but using the new string/boolean lists. Fair warning a lot of the functions you might try to use will cause `SQL` errors, give them a go but when you inevitably get frustrated a combination of `filter` and `count` on a string list can get you there. You then need to add in the mappings for the `MilkDrinkList` and the `IcedDrinkList` and as these are now both lists we will have the correct number of `Tea` classes. Now once these derived properties and mappings are finished the `agg` tests should now pass, you will notice however that the JSON for the derived properties has `()` on the end due to them being derived. It is worth noting that I think the derived properties are generated using `SQL` rather than mapping and then deriving, which is why a large chunk of the functions don't quite do what you would expect.