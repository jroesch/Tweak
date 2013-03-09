#Tweak
A simple embeddable scripting language for experimentation with dynamic adjustment of static code. The goal is to create an environment for people to visually interact with things in a tactile way. Tweak is intended to be a the component that allows for effortless 'tweaking' of parameters like color, time, motion, animation, and so on. Part of this motivation is why it is initially targeting the JVM for ease of binding to GUI toolkits like Swing, and utilize lower-level high performant code in the form of Scala and Java with bindings such as LWGL.

Although the above was the intended trajectory for the project we are drivin ahead on integrating this to build web serivces in one language, and do special conditional compilation to Scala, and JavaScript, providing a truly concurrent backend, and a opaque connection between the two. As well as some of our original. 

#Rough TODO
## Finish Basic Parser
## Finish AST Generation
## Finish AST Evaluation
## Finish Runtime
## Work on Direct Tweak to Scala compilation for performance 
## Work on Interop. 
## Work on binding basic IO, and basic std lib functions

# Libraries in use
- GLL Combinators(Parser Combinator Library)
- Specs2 with ScalaCheck (for testing)

