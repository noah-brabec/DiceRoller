# DiceRoller

This is a dice roller made for EECS 776. I plan to use it when playing dungeons and dragons in the future.

## Features

### Single Dice Rolling
 The users will have the option to roll a single dice by clicking on a button. This will automatically send a roll like "1d4" to the api 

### Dice Strings
There will also be a text field that the user can enter dice strings into. For example, if the user wanted 3 four sided dice added to 5 twenty sided dice then they would enter "3d6+5d20".

### Api Access
Along with the UI with the pictures of dice, the user can also just make api calls to the endpoint that the front end uses. This will be located at [websitename.com]/roll/<diceString>.