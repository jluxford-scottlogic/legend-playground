## SoftDrink Mappings: Example 5

Copy the `5_SoftDrink.pure` text into your legend text editor to try this example. This will be using some new `Factory` and `Drink` classes. There are also some other `Class`es and `Association`s that will need to be mapped as well, it is worth noting that the `Drink` here is a super type of the `FizzyDrink` and `IndustrialDrink` which means that all of the properties of the `Drink` class are on the child classes. The associations on the other hand overwrite the association that links `Drink` to `Company` which means that the mappings can work a bit easier.

### Testing

To test your code we will use some pre-defined tests which will take in a given JSON and expect an output JSON. Once the mappings are complete all of the tests should then pass.

# TODO

### Basic Mappings

First adjust all of the mapped classes to have their corresponding basic `Id`s. Once this is done the tests should now have all of the `class_id` tests pass and when you go through adjusting the association mappings the tests will then pass. You should notice that the `industrial_drink_id` test is failing despite adding in the `id`, invistigate why this test is failing and adjust the mapping so that this test passes.

### Drink

Adjust the `drink` mapping to convert from the `FactoryLine` to the `Drink` class. Sometimes you won't need to assign all of the associations and this mapping is an example where the association will only need to happen in one direction.

### IndustrialDrink

Adjust the `drink` mapping to convert from the `FactoryLine` to the `IndustrialDrink` class. This will require adding in a `filter` to make it so that only drinks of at least 1000 in size are mapped to and `IndustrialDrink`.

### FizzyDrink

Adjust the `drink` mapping to convert from the `FactoryLine` to the `FizzyDrink` class. This will require adding in a `filter` to make it so that only the fizzier of the drinks options are mapped across (specifically lemonade, cola and ginger beer).

### Company

Adjust the `Company` mapping so that it only puts the `FactorySpecs` into the new class, ideally this mapping should be simple as the previous filters will handle any of the complications for you. You should notice that the `Drink` mapping is being greedier than it should, this is because as legend is behaving functionally (i.e. there is no `else` cases) and so will also need a mapping that excludes the fizzy and industrial cases. Add this in so that the `Company` tests pass.