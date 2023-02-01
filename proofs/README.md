Lurk Proofs of Retirement Data
------------------------------

An experiment to use [lurk](https://lurk-lang.org) to provide arbitrary data transformations over our data
backed by zero-knowledge proofs.

First install `fcomm` which allows us to generate functional commitments.

```
git clone https://github.com/lurk-lang/lurk-rs /path/to/lurk-rs
cargo install --path /path/to/lurk-rs/fcomm
```

In the `data` directory is a simplified piece of private data with a name, an age and some flights. Our
auditible data should scrub the name and age but leave the flights. This is exactly what `private-to-audit.lurk`
does in a very simple way.

With our program in hand, we can now commit to it to it.

```
fcomm commit --function private-to-audit.lurk --lurk
```

For some reason this overwrites your file so I've already generated it and added it here in the file `pta-function.json`.
Next we want to convert our private data from JSON to lurk, I wrote some OCaml to do that.

```
make pr1.lurk
```

You will need to cd into `lurk_of_json` to build the OCaml code first. Now we can create an opening:

```
make pta-open-proof-pr1.json
```

And finally we can verify the opening.

```
make verify-pta-open-proof-pr1
```
