## Drink Mappings: Example 3

Copy the `3_Drink.pure` text into your legend text editor to try this example. This will be using two `Drink` classes, one of which will be a `DrinkSource` which will be the source of the mapping and the other will be a `Drink` which is the destination. There are also some `Enum`s that will need to be mapped as well. To create these you will need to make a new mapping within the `drink` mapping.

### Testing

To test your code we will use some pre-defined tests which will take in a given JSON and expect an output JSON. Once the mappings are complete all of the tests should then pass.

### Enum Mapping 1

Make a new mapping within the `drink` mapping which maps the `DrinkTypeSource` to a `DrinkType`, this should hopefully be a fairly simple enumeration mapping however you won't be able to test it until you reach the final mapping.

### Enum Mapping 2

Make a new mapping within the `drink` mapping which maps the `DrinkTypeSource` to a `DrinkStat`.

## Class Mapping 1

Finally adjust the `Drink` mapping within the `drink` mapping to convert from the `DrinkSource` to the `Drink` class. This will require making use of the two Enum mappings that you have done above. Once this is complete all of your tests should pass.