# hs-scheme

# How to use this?

```bash
stack run <arg1> 
```
<arg1> can be either a string, a number, an atom, a list or a dotted list. For example:

```bash
> stack run 12
12
> stack run "#atom!"
#atom!
> stack run \"hello\"
"hello"
> stack run "(my list)"
(my list)
> stack run "(dotted . list)"
(dotted . list)
```

Note that in order to parse Strings you need to explicitely escape the '"' characters. Otherwise they will be parsed as atoms. 

It can also support basic arithmetic operations for both integers, strings and bools, as well as if-else clauses: 

```
> stack run "(+ 1 2)"
3
> stack run "(- 1 2)"
-1
> stack run "(+ "\1\" 2)"
-1
> stack run "(if (< 2 3) \"first is smaller\" \"second is smaller\")"
"first is smaller"
```

Running without arg will result in an error message being thrown:

```bash
> stack run 
Hey! Give me something to work with
```

# Testing

To run tests use `stack test`
