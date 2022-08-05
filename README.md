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
