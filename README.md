# Annodate

## Usage

``` sh
Usage: ad [-c|--color ARG] [-C|--no-color] [-f|--format ARG]
          [-i|--inactivity-timeout ARG] [-P|--no-pause]
  Annodate - Prepend timestamps to stdio

Available options:
  -c,--color ARG           Format using color
  -C,--no-color            Do not use colors
  -f,--format ARG          Specify timestamp format (strftime)
  -i,--inactivity-timeout ARG
                           Threshold for pause in the input in seconds
  -P,--no-pause            Disable showing pauses in the input
  -h,--help                Show this help text
```

## Example

```sh
❯ cabal build | ad
2021-04-12 08:51:15: Resolving dependencies...
2021-04-12 08:51:15: Build profile: -w ghc-8.10.4 -O1
2021-04-12 08:51:15: In order, the following will be built (use -v for more details):
2021-04-12 08:51:15:  - annodate-0.1.0.0 (lib) (first run)
2021-04-12 08:51:15:  - annodate-0.1.0.0 (exe:ad) (first run)
2021-04-12 08:51:15: Configuring library for annodate-0.1.0.0..
...
```

![annodate-ping](https://user-images.githubusercontent.com/148158/114354132-1c64ad80-9b6e-11eb-8e31-413b09e402d0.gif)
