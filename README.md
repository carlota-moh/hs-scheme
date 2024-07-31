# hs-scheme

# How to use this?

```bash
stack run <arg1> <arg2>
```
It will parse the head of each argument. If it happens to be a symbol, it will return a match. Otherwise, it will output an error. For example:

```bash
> stack run hello there

No match: "lisp" (line 1, column 1):
unexpected "h"
No match: "lisp" (line 1, column 1):
unexpected "t"
```

```bash
> stack run + $

Found value! '+'
Found value! '$'
```
