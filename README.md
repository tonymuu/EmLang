# EmLang

## What is EmLang?
EmLang stands for “Emoji Language”. It is a proof of concept. It proves that programming languages are formal. And the symbols themselves don't matter. We can use any symbol (Emoji included) to code. 

EmLang is implemented in Haskell. It is a project I made for UIUC CS421 Programming Languages and Compilers. It is based heavily on the Forth language, which you can check out here https://www.forth.com/forth/. EmLang borrowed the framework and starter code from one of the MPs we did for this class. The original MP source code is confidential.  

## How to Run?
Setting up is very easy if you use Docker. 

* Pulled the latest Haskell image: ```docker pull haskell```
* Start the container based on that image
* Clone this repository inside of a directory in the container
* Run ```stack repl``` to start the Haskell repl
* Enter ```main``` and hit enter to start the EmLang compiler
* Viola! ![image](https://user-images.githubusercontent.com/10318596/183007590-1f741d57-3735-418b-9e48-44eef6bb4a20.png)

## How to Test?
Instead of running ```stack repl```, run ```stack test``` which will run all of the test cases.


## Sample programs
### How to Print

Printing is easy in EmLang! Just do 🖨️ to print and pop the top of the stack, or 🖨️🎉 to print the entire stack!

For example, to print the current stack:

![image](https://user-images.githubusercontent.com/10318596/183010336-acb6d86f-d928-4f4e-8b8f-124d365e548d.png)

To print and pop the top element of the stack:

![image](https://user-images.githubusercontent.com/10318596/183010407-88944da2-1728-423e-8fd8-884657776cf1.png)

And now the stack looks like this:

![image](https://user-images.githubusercontent.com/10318596/183010458-f3242e3a-7f19-45e8-946d-e56420d7b406.png)


### Define a "max" function

The following block of code defines a "max" function which returns the biggest of the two give integers.

```✏️📖 max 🪞 ☀️ 🪞 ☀️ 😀⚖️ 🤔 🙃 🤖 💦 📕```

Running the max function:

![image](https://user-images.githubusercontent.com/10318596/183010627-488ffc80-e6df-4326-8361-1d047aaa13dc.png)

Of course, since this is EmLang, it is recommended that you define functions in Emojis too! We should define the above max function with the 🐶 emoji:

```✏️📖 🐶 🪞 ☀️ 🪞 ☀️ 😀⚖️ 🤔 🙃 🤖 💦 📕```

And the results of calling our new 🐶 function is:

![image](https://user-images.githubusercontent.com/10318596/183010924-be272115-6648-41bb-917e-6d1d76f75998.png)


Happy coding in EmLang!
