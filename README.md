# An interpreter of (a subset of) the Scheme programming language

Written in Haskell.

Copyright Martin Trojer <martin.trojer@gmail.com>

## Usage

```
$ stack exec mtscheme-exe
Welcome to mtscheme v0.1
> (define (map f l) (if (not (null? l)) (cons (f (car l)) (map f (cdr l)))))
nil
> (map (lambda (x) (* x x)) (list 1 2 3))
[1.0,4.0,9.0]
>
```

## Tests

```
$ stack test
```

## License

GPLv3
