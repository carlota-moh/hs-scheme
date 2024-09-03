# hs-scheme

# How to use this?

```bash
stack run <arg1> 
```
<arg1> can be either a string, a number, an atom, a list or a dotted list. For example:

```bash
> stack run 12
Found value! 12
> stack run "#atom!"
Found value! #atom!
> stack run \"hello\"
Found value! "hello"
> stack run "(my list)"
Found value! (my list)
> stack run "(dotted . list)"
Found value! (dotted . list)
```

Note that in order to parse Strings you need to explicitely escape the '"' characters. Otherwise they will be parsed as atoms. 

Running without arg will result in an error message being thrown:

```bash
> stack run 
Hey! Give me something to work with
```

# Testing

To run tests use `stack test`
