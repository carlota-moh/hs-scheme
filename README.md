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

It can also support basic arithmetic operations for both integers and strings (due to weak typing): 

```
> stack run "(+ 1 2)"
3
> stack run "(- 1 2)"
-1
> stack run "(+ "\1\" 2)"
-1
> stack run "(* 1 2)"
2
> stack run "(/ 10 2)"
5
> stack run "(quotient 10 3)"
3
> stack run "(mod 10 3)"
1
> stack run "(remainder 10 3)"
1
```

Running without arg will result in an error message being thrown:

```bash
> stack run 
Hey! Give me something to work with
```

# Testing

To run tests use `stack test`
