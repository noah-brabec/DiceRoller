# DiceRoller

This is a dice roller made for EECS 776. I plan to use it when playing dungeons and dragons in the future. Currently this directory only contains haskell code, but after finals are finished am planning on putting together a simple front end in elm.

## Features

This project was made with the Haskell Stack tool chain. To build, download this directory and run `stack build`. This will automatically download and packages missing for this project. To execute the project run the command `stack exec DiceRoller-exe`. This will start the project and you will now be able to enter dice string at port 3000 of your local host. The output will be a touple, one element is the total of the rolls and the other is each roll represented as the dice that was rolled and its output. For more information on Haskell Stack visit: https://docs.haskellstack.org/en/stable/GUIDE/

### Dice Strings
There will also be a text field that the user can enter dice strings into. For example, if the user wanted 3 four sided dice added to 5 twenty sided dice then they would enter "3d6+5d20".

### Api Access
Along with the UI with the pictures of dice, the user can also just make api calls to the endpoint that the front end uses. This will be located at localhost:3000/<diceString>. Currently this is the only way to interface with the program.