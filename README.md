
Cirru Parser in Racket
---

Converted from CoffeeScript.

### Usage

```text
raco pkg install git://github.com/Cirru/parser-rkt
```

```rkt
#lang racket
(require cirru-parser)

(cirru-parser-parse "code" "file")
(cirru-parser-pare "code" "file")
```

### Development

Run this as a test:

```text
racket test.rkt
```

### License

MIT
