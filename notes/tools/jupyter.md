# Jupyter OCaml Kernel

Refs: [OCaml Jupyter](https://akabe.github.io/ocaml-jupyter/)

### Mac OS

- `brew install zlib libffi gmp zeromq`
- `pip3 install jupyterlab`
- `export PATH="$HOME/.local/bin:$PATH"`
- `opam install jupyter` # make sure there are no installation errors, otherwise, the following command won't work
- `ocaml-jupyter-opam-genspec`  # generate json file
- `grep topfind ~/.ocamlinit || echo '#use "topfind";;' >> ~/.ocamlinit  # For using '#require' directive
- `grep Topfind.log ~/.ocamlinit || echo 'Topfind.log:=ignore;;' >> ~/.ocamlinit  # Suppress logging of topfind (recommended but not necessary)`
- `jupyter kernelspec install --user --name "ocaml-jupyter-$(opam var switch)" "$(opam var share)/jupyter"`  # with `--user` or `sudo`

If above doesn't work:

```
$ pip3 show jupyter | grep location
$ cd location # location received from above command
$ python3 jupyter.py kernelspec install --user  --name "ocaml-jupyter-$(opam var switch)" "$(opam var share)/jupyter"
```
