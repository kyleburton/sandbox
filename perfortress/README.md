# perfortress

A Clojure library to streamline and simplify working with the Perforce Java SDK.

## Installation

In your project.clj file, add the following to ``:dependencies```

```clojure
  [com.github.kyleburton/perfortress "0.1.0"]
```

To build and run the docker image, download [bake](https://github.com/kyleburton/bake) and put it on your PATH.  Then run ```bake```

```bash
> bake

/Users/kburton/bin/bake task [arg ...]

  build                          Build the docker image
  docker-logs                    Show the logs for the running container
  docker-restart                 Restar the container
  docker-shell                   Open a shell on your container.
  docker-start                   Start your container
  docker-status                  See if the container is running
  docker-stop                    Stop your container
  p4-init                        Bootstrap the p4 instance
  run-p4                         Run a p4 comand in the context of the docker container

> bake build
...
> bake docker-run
> bake p4-init
> bake run-p4 info
> bake run-p4 users
> bake run-p4 ... other p4 commands ...
```

## Usage


```clojure
;; examples go here
```

## License

Copyright © 2016 Kyle Burton &lt;kyle.burton@gmail.com&gt;

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
