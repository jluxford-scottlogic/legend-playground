## Coffee: Example 1

This is a basic modelling example that will attempt to get you familiar with creating classes, associations and enums. To that end we will need to create a basic workspace and legend studio environment before then creating a basic model situation.

### Importing

Make a new workspace and then open the `1_Coffee.pure` file that is part of this playground. You will see a pure instance that contains a single `Class`, `Bean` which is a representation of a bag of coffee beans. This class contains a few different properties:
- `BagId`: The individual identifier of the bag
- `Roast`: The roast level of the bag of beans
- `OriginLocation`: The location of where the beans were grown

This provides a very basic instance of a class, to add this to your workspace, change to text mode and then copy and paste this class in. This will then create a folder structure based off of a `package::package::File` structure. This will then be compiled for the ui to create the folder strucutre but the text will all be one single file.

Once this has been imported, you should see the class within the folder structure and you can then move onto the next steps of the playground.

### Testing

To test your code throughout example 1 you will be required to copy and paste your full text instance into the `1_Coffee_test.pure` file and then run the `1_Coffee_test.exe` file which will attempt to parse your pure file and check to see if you have done the exercises correctly. It will output the result by overwritting the `1_Coffee_test_result.txt` file, so if you would like previous result instances simply change the name before running the next test.

### Classes

Next we will want to create a new `Class` which will be a new `Location` class, it should have the following properties:
- Name
- Country

Both of which should be single instance `Strings` that should always be present.

### Enums

Currently we have a `Roast` property on our beans which could be any value as it is a `String` however we would like this to be limited to the different roast types of beans, specifically:
- Light
- Dark
- Raw

Make a new `Enumeration`/`Enum` called `RoastStrength` which only allows for those specific values and then change the type of the `Bean`s `Roast` property to use this new `Enum`.

### Associations

Our `Bean` class has an `OriginLocation` value which should directly tie to an actual `Location`. To get these to actually link across such that a `Bean` class instance will link to a `Location` class instance we will need to make an `Association`. This will replace the need for an `OriginLocation` value, so first thing is to remove the `OriginLocation` value from `Bean`.

Add a new `Association` `BeanLocation` which links a single bag of `Bean` instance to a `Location` such that each `Location` could have multiple bags of beans as their `OriginLocation`.

### Company

Now let's try adding in a completely new `Class` which has a new `Association` that links itself to some `Bean`s. This is going to be a `Company` class which will need the following:
- `CompanyId` should be an `Integer`
- `Roaster` should be a `Boolean`

The `Association` `CompanyBeans` should link `Company` to `Bean` and each `Company` should have any number of bags of `Bean`s available to it but each bag should only be owned by one company at a time.