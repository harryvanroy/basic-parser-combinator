# JSON parser + formatter written in Haskell

A simple JSON parser which uses a parser combinator written from scratch. This
project served as a way for me to learn more about functional parsing.

### Features

- Formatted JSON with indendation and sorted keys.

### Usage

```
cabal run json-parser -- file
```

### Example formatting

Before:

#### **`test.json`**

```
{"c":[1,2,3],"b":{"e":1,"d":2},"a":3}
```

After:

#### **`test.json`**

```
{
  "a": 3,
  "b": {
    "d": 2,
    "e": 1
  },
  "c": [
    1,
    2,
    3
  ]
}
```

### Example parsing fail

#### **`test.json`**

```
{"c:[1,2,3],"b":{"e":1,"d":2},"a":3}
```

```Bash
cabal run json-parser -- "test.json"
Parser failed at char 1: Expected char '}' but found "c:[1,2,3],"b":{"e":1,"d":2},"a":3}
```
