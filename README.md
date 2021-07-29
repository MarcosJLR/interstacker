# intestacker
An interpreter for a stack machine programming language.

## Instructions
To run the interpreter you must, while being on the root directory
of this project, run
```bash
stack build
```

then run
```bash
stack run -- <program>
```

where `program` is the filen of the program that
wants to be interpreted.

The program must have the correct syntax or else it will
throw an error and wont be executed. You can run the
sample located at `./samples/example.stk` with the following command

```bash
stack run -- ./samples/example.stk
```
