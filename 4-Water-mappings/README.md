## Water Mappings: Example 4

Copy the `4_Water.pure` text into your legend text editor to try this example. This will be using some `Water` classes, one of which will be a `WaterSource` which will be the source of the mapping and the other will be a `WaterBottle` which is the destination. There are also some other `Class`es and `Association`s that will need to be mapped as well.

### Testing

To test your code we will use some predefined tests which will take in a given JSON and expect an output JSON. Once the mappings are complete all of the tests should then pass.

# TODO

### Basic Mappings

First adjust all of the mapped classes to have their corresponding basic `Id`s. Once this is done the tests should now have all of the `class_id` tests pass and when you go through adjusting the association mappings the tests will then pass.

### Mineral

Adjust the `Mineral` mapping to convert from the `WaterSource` to the `WaterBottle` class. To properly map an association you need to have the class being passed in also mapped which is why the `WaterBottleId` mapping needed to happen first.

### Location

Adjust the `Location` mapping so that it only puts the `WaterSource` into the current "sent" or "received" based on whether this `Location` is a destination or not. You can use an `[]` as an empty value.

## Water Bottle

Finally adjust the `WaterBottle` mapping to use the `Mineral`s mapping and then also take from the locations the destination and source locations for the other two mappings.
