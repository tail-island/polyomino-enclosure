# polyomino-enclosure

A test environment for "eclosing by polyominos" puzzle.

## Usage

### Generate Clojure and ClojureScript from *.cljx.

````shell
$ lein cljx once
````

### Generate JavaScript from ClojureScript.

````shell
$ lein cljsbuild once
````

### Open `resources/public/index.html`

Check rules of this puzzle.

### Generate jar

```shell
$ lein uberjar
```

### Execute on console.

```shell
$ java -jar target/polyomino-enclosure-0.1.0-SNAPSHOT-standalone.jar question-file answer-file
```

Check your question and answer files' correctness.

## License

Copyright © 2015 OJIMA Ryoji.

Distributed under the Eclipse Public License either version 1.0 or any later version.
