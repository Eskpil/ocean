# Ocean

A compiled programming language i make cause languages are fun.

## Example

```
tests/struct.on

struct Person {
    name: String
    age: Int
}

fn program() {
    let person = Person[name: "John", age: 12];
}
```

## I want: 

1. Reference counting for garbage collection.
2. Curly bracket based syntax since my friend said so.
3. And very cool runtime like go has.

## I have:

1. Crappy reference counting.
2. Curly bracket based syntax.

## I don't have:

1. Cool runtime like go.
2. Neat features such as += or general assignment.

## Build:


### Note:

The compiler requires some binaries to be present on the system.

1. **clang**
2. **rustc**

Since i am loyal to my own work i decided to use my own build system 
[Chain](https://github.com/Eskpil/chain). 

Once Chain is install this should do the trick.

```
chain run ./project.yml
```

## Run 

I wanna compile some code, tell me how.

```
./bld/oceanc/oceanc ./tests/{test}.on
```
